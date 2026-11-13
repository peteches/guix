#!/usr/bin/env python3
"""transcriber — WhisperX transcription + speaker diarization API.

Single-file FastAPI app served by (peteches services transcriber) inside the
uv-managed venv on the comfyui VM's RTX 4090.  Deliberately self-contained
(no package structure): the Guix service references this file directly via
local-file, so editing it here + redeploying is the whole release process.

Diarization is done by the sibling file transcriber-diarizer.py (NVIDIA
SortFormer v2.1 in ONNX, run through onnxruntime on CPU), which the Guix
service installs next to this one and passes via --diarizer-file.

Endpoints:
  GET  /            minimal upload UI (file -> speaker-labelled transcript)
  POST /transcribe  multipart `file` + optional `format` (json|srt|vtt|txt),
                    `language` (ISO code, default auto-detect), `diarize`
                    (bool, default true), `summarize` (bool, default false:
                    ask the local strata LLM for minutes + task allocation),
                    `save` (bool, default false: write transcript and summary
                    under --summary-dir), `speaker_labels` (comma-separated
                    names for the voices, in order of first speech).
                    Saved files come back as X-Transcript-Path /
                    X-Summary-Path headers.
  GET  /health      liveness + which models are resident + what diarization
                    and summarization are wired to

Design notes:
  - Models are LAZY-LOADED on first request and UNLOADED after an idle
    timeout (default 10 min): the 4090 is shared with ComfyUI (~13.5GB) and
    AllTalk/XTTS (~2.2GB), so this service must not squat ~4GB of VRAM
    between transcriptions.  Idle cost is just the Python process.
  - One GPU worker at a time: a module-level lock serialises requests
    (faster-whisper batching already saturates the card; queueing beats
    OOMing when ComfyUI is mid-workflow).
  - Diarization carries no HuggingFace gate and no token.  whisperx's own
    diarizer (pyannote/speaker-diarization-community-1) is `gated=auto`, so a
    diarized transcript would need the operator to accept that model's terms
    and ship an HF token into the VM; NVIDIA's SortFormer ONNX export is
    ungated, downloads anonymously, and onnxruntime was already in the venv.
    It runs on CPU, which also keeps the 4090 free for ASR.  The checkpoint is
    fetched once into --sortformer-model with its SHA-256 verified.
  - Voices are labelled SPEAKER_01, SPEAKER_02 ... in order of first speech.
    SortFormer's arrival-order speaker cache gives stable identity by
    construction, but a *second slot* can be the same person under a different
    acoustic condition rather than a second person — which is why slots whose
    mean embeddings are at least --merge-sim similar get merged and why the
    similarity matrix is returned for audit.  On the reference meeting file the
    two slots that turned out to be one voice scored 0.985 against each other
    and 0.771 against the other participant, so the default threshold sits well
    inside a wide empty band.  `speaker_labels` renames voices in order of first
    speech: the model knows voices, never names.
  - Speaker labels are attached to whisperx's *aligned words* (maximum
    time-overlap wins; midpoint/nearest-segment fallback within
    --assign-window), then consecutive same-speaker words are grouped into
    utterances, splitting on speaker change or a silence gap longer than
    --utterance-gap.  Word level matters: on the reference file 48 of 282
    whisperx segments straddled a turn change, and labelling those segments as
    a unit would misattribute about a fifth of the transcript.
  - Summarization (optional) posts the labelled transcript to strata's
    OpenAI-compatible endpoint on the same host and asks for markdown minutes:
    summary, decisions, agreed tasks with owners, open questions and a
    per-person action list.  It runs *outside* the GPU lock so a slow LLM never
    blocks transcription, and its failure never loses the transcript.
  - SRT/VTT are rendered here (with speaker labels) rather than via whisperx's
    SubtitlesProcessor, to avoid its nltk punkt dependency.
"""

import argparse
import bisect
import hashlib
import html
import re
import json
import os
import threading
import time
from pathlib import Path

import torch
import uvicorn
import whisperx
from fastapi import FastAPI, File, Form, HTTPException, UploadFile
from fastapi.responses import HTMLResponse, JSONResponse, PlainTextResponse, Response

# ── CLI / globals ──────────────────────────────────────────────────────────

# SortFormer ONNX export of NVIDIA's ungated diar_streaming_sortformer_4spk-v2.1.
# Digest pinned from the file this backend was validated against (cgus' export,
# NVIDIA Open Model License) so a swapped artifact fails loudly instead of
# silently diarizing worse.
SORTFORMER_NAME = "diar_streaming_sortformer_4spk-v2.1.onnx"
SORTFORMER_URL = ("https://huggingface.co/cgus/"
                  "diar_streaming_sortformer_4spk-v2.1-onnx/resolve/main/"
                  + SORTFORMER_NAME)
SORTFORMER_SHA256 = ("82b9c735e1cfc6b36b4ff8a994d9a0573e922d0e80a58a8553b2"
                     "c58f7aff0c00")

parser = argparse.ArgumentParser(description="WhisperX transcriber service")
parser.add_argument("--port", type=int, default=8787)
parser.add_argument("--host", default="0.0.0.0")
parser.add_argument("--model-size", default="large-v3")
parser.add_argument("--compute-type", default="float16")
parser.add_argument("--batch-size", type=int, default=4)
parser.add_argument("--idle-unload-minutes", type=float, default=10.0)
parser.add_argument("--max-upload-mb", type=int, default=500)
parser.add_argument("--uploads-dir", default="/var/lib/transcriber/uploads")

# Diarization (transcriber-diarizer.py + NVIDIA SortFormer v2.1 ONNX).
# --diarizer-file's default resolves inside a repo checkout, where both files
# are siblings; the Guix service always passes its own store path explicitly,
# because each local-file lands in its OWN store directory.
parser.add_argument("--diarizer-file",
                    default=os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                         "transcriber-diarizer.py"))
parser.add_argument("--sortformer-model", default="",
                    help="path to diar_streaming_sortformer_4spk-v2.1.onnx "
                         "(default: <uploads-dir>/../onnx/<name>)")
parser.add_argument("--sortformer-url", default=SORTFORMER_URL)
parser.add_argument("--sortformer-sha256", default=SORTFORMER_SHA256,
                    help="expected digest of the download; empty disables it")
parser.add_argument("--merge-sim", type=float, default=None,
                    help="cosine similarity above which two slots are treated "
                         "as one voice (default: the diarizer's own)")
parser.add_argument("--min-speaker-seconds", type=float, default=None,
                    help="ignore diarization slots quieter than this over the "
                         "whole file (default: the diarizer's own)")
parser.add_argument("--diarizer-threads", type=int, default=0,
                    help="onnxruntime intra-op threads (0 = runtime default)")
parser.add_argument("--assign-window", type=float, default=1.0,
                    help="seconds of slack for labelling a word by the nearest "
                         "diarization segment")
parser.add_argument("--utterance-gap", type=float, default=2.0,
                    help="split an utterance after this much silence")

# Summarization backend: strata on comfyui (OpenAI-compatible), loopback.
parser.add_argument("--summarizer-base-url", default="http://127.0.0.1:8000")
parser.add_argument("--summarizer-model", default="",
                    help="model id; empty = ask the server (/v1/models)")
parser.add_argument("--summary-dir", default="/var/lib/transcriber/meetings")
ARGS = parser.parse_args()

UPLOADS = Path(ARGS.uploads_dir)
UPLOADS.mkdir(parents=True, exist_ok=True)

DEVICE = "cuda" if torch.cuda.is_available() else "cpu"

# ── Lazy model cache with idle unload ─────────────────────────────────────

class ModelCache:
    def __init__(self):
        self._lock = threading.Lock()          # serialises GPU work
        self._asr = None
        self._align = {}                        # language -> (model, meta)
        self._diar = None                       # SortFormer diarizer (CPU/ONNX)
        self._last_use = time.monotonic()
        self._timer = None

    # -- loading -------------------------------------------------------------
    def asr(self):
        if self._asr is None:
            self._asr = whisperx.load_model(
                ARGS.model_size, device=DEVICE, compute_type=ARGS.compute_type
            )
        return self._asr

    def align(self, language):
        if language not in self._align:
            self._align[language] = whisperx.load_align_model(
                language_code=language, device=DEVICE
            )
        return self._align[language]

    def diarizer(self):
        """The SortFormer diarizer, loaded on the first diarized request.

        CPU-only, so the 4090 stays reserved for ASR.  Idle-unloaded with
        everything else: reloading means re-reading a ~470 MB ONNX file (a
        couple of seconds), which is cheaper than holding that much host RAM
        between transcriptions on a box also running ComfyUI.
        """
        if self._diar is None:
            self._diar = load_diarizer_module().SortformerDiarizer(
                ensure_sortformer_model(),
                intra_op_threads=(ARGS.diarizer_threads or None))
        return self._diar

    # -- lifecycle -----------------------------------------------------------
    def touch(self):
        self._last_use = time.monotonic()
        self._arm_timer()

    def _arm_timer(self):
        """Ensure an unload timer is pending, sized to the remaining idle time.

        Callers may hold self._lock; this never takes it (the timer thread calls
        it from inside the lock).
        """
        if ARGS.idle_unload_minutes <= 0 or self._timer is not None:
            return
        delay = max(1.0, ARGS.idle_unload_minutes * 60
                    - (time.monotonic() - self._last_use))
        timer = threading.Timer(delay, self._unload_if_idle)
        timer.daemon = True
        self._timer = timer
        timer.start()

    def _unload_if_idle(self):
        with self._lock:
            self._timer = None
            if time.monotonic() - self._last_use < ARGS.idle_unload_minutes * 60:
                # Woke early: a request touched after this timer was armed (a
                # single request touches twice, so arming at the first touch puts
                # the deadline before the second one).  Sleep out the remainder
                # instead of dropping the timer -- nothing else would re-arm it
                # while the service stays idle, and VRAM would never be freed.
                self._arm_timer()
                return
            if self._asr is None and not self._align and self._diar is None:
                return                                  # already unloaded
            self._asr = None
            self._align = {}
            self._diar = None
            if DEVICE == "cuda":
                try:
                    torch.cuda.empty_cache()
                except Exception as exc:      # context gone: nothing to free
                    print(f"[transcriber] idle timeout: empty_cache failed {exc!r}",
                          flush=True)
            print("[transcriber] idle timeout: models unloaded, VRAM freed",
                  flush=True)

    def loaded(self):
        return self._asr is not None

    def diarizer_loaded(self):
        return self._diar is not None


CACHE = ModelCache()


# ── Diarization backend plumbing ───────────────────────────────────────

_DIARIZER_MOD = None


def load_diarizer_module():
    """Import transcriber-diarizer.py by path (a store FILE, not a package)."""
    global _DIARIZER_MOD
    if _DIARIZER_MOD is None:
        import importlib.util
        spec = importlib.util.spec_from_file_location("transcriber_diarizer",
                                                      ARGS.diarizer_file)
        if spec is None or spec.loader is None:
            raise RuntimeError(f"cannot load diarizer module {ARGS.diarizer_file!r}")
        mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(mod)              # numpy + onnxruntime import here
        _DIARIZER_MOD = mod
    return _DIARIZER_MOD


def sortformer_model_path():
    if ARGS.sortformer_model:
        return ARGS.sortformer_model
    return str(Path(ARGS.uploads_dir).parent / "onnx" / SORTFORMER_NAME)


def sortformer_fetchable():
    """Could we put the ONNX file where --sortformer-model points?"""
    path = Path(sortformer_model_path())
    if path.is_file():
        return True
    parent = path.parent
    while not parent.exists() and parent.parent != parent:
        parent = parent.parent
    return os.access(str(parent), os.W_OK)


def ensure_sortformer_model():
    """Return the SortFormer ONNX path, downloading it on first use.

    Shaped like the whisper models: lazily fetched into a cache directory on the
    data volume, anonymously (NVIDIA's checkpoint is ungated), and only renamed
    into place once the pinned SHA-256 matches.  The caller holds CACHE._lock,
    which also stops two queued requests racing each other for the download.
    """
    path = Path(sortformer_model_path())
    if path.is_file():
        return str(path)

    import requests

    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_name(path.name + ".part")
    started, digest = time.time(), hashlib.sha256()
    print(f"[transcriber] fetching {SORTFORMER_NAME} (~470 MB) ...", flush=True)
    with requests.get(ARGS.sortformer_url, stream=True, timeout=120) as r:
        r.raise_for_status()
        with open(tmp, "wb") as fh:
            for chunk in r.iter_content(1 << 20):
                digest.update(chunk)
                fh.write(chunk)
    got = digest.hexdigest()
    if ARGS.sortformer_sha256 and got != ARGS.sortformer_sha256:
        tmp.unlink(missing_ok=True)
        raise RuntimeError(f"SortFormer digest {got} != expected "
                           f"{ARGS.sortformer_sha256} (from {ARGS.sortformer_url})")
    os.replace(tmp, path)
    print(f"[transcriber] fetched {path} in {time.time()-started:.0f}s",
          flush=True)
    return str(path)


# ── Summarization via strata (OpenAI-compatible endpoint) ───────────────

SUMMARY_PROMPT = """\
You are writing the minutes of a recorded meeting. The transcript below is
speaker-labelled: every line starts with that speaker's name. Trust those
labels for attribution — they come from an audio diarizer, not from guessing.

Reply in Markdown with exactly these sections:

# Meeting summary
Two or three short paragraphs on what was discussed and why it mattered.

# Decisions
Bullet list of what was actually agreed, attributing each to who agreed it. If a
decision was provisional or was reversed later in the recording, say so.

# Agreed tasks
A table with columns Owner | Task | Evidence/notes, one row per task. Only list
tasks somebody committed to on the recording — not things that were merely
mooted. Use the speaker labels as owners; write "unassigned" when a task was
agreed but nobody took it, and quote what was said about who should.

# Open questions
Bullet list of unresolved points, each attributed to whoever raised it.

# Per-person commitments
One subsection per speaker ("## <name>"), listing only that person's own
commitments. Write "none" if they committed to nothing.

Rules: base everything on the transcript — never invent an owner, a deadline or
a decision. Quote short phrases where the exact wording matters (promised vs.
floated). Where two speakers contradict each other, record both readings instead
of silently picking one. Keep the whole document under 900 words.

Transcript:
"""

_SUMMARIZER_MODEL = None


def summarizer_model():
    """Summarizer model id: configured if given, otherwise discovered once."""
    global _SUMMARIZER_MODEL
    if ARGS.summarizer_model:
        return ARGS.summarizer_model
    if _SUMMARIZER_MODEL is None:
        import requests
        r = requests.get(ARGS.summarizer_base_url + "/v1/models", timeout=30)
        r.raise_for_status()
        ids = [m.get("id") for m in r.json().get("data", []) if m.get("id")]
        if not ids:
            raise RuntimeError(f"{ARGS.summarizer_base_url} advertises no models")
        _SUMMARIZER_MODEL = ids[0]
    return _SUMMARIZER_MODEL


def summarize_transcript(labelled_text):
    """Ask strata for minutes + task allocation; returns markdown."""
    import requests

    body = {"model": summarizer_model(), "stream": False, "temperature": 0.2,
            "max_tokens": 4096,
            "messages": [{"role": "user", "content": SUMMARY_PROMPT + labelled_text}]}
    r = requests.post(ARGS.summarizer_base_url + "/v1/chat/completions",
                      json=body, timeout=900)
    if r.status_code >= 400:
        raise RuntimeError(f"HTTP {r.status_code}: {r.text[:300]}")
    data = r.json()
    choices = data.get("choices") or []
    if not choices:
        raise RuntimeError(f"no choices in response: {str(data)[:200]}")
    content = (choices[0].get("message") or {}).get("content")
    if isinstance(content, list):             # some servers chunk the body
        content = "".join(p.get("text", "") if isinstance(p, dict) else str(p)
                          for p in content)
    if not content:
        raise RuntimeError("empty assistant message")
    return content.strip()


# ── Pipeline ───────────────────────────────────────────────────────────────

def run_pipeline(path, language, diarize, speaker_labels=None):
    """Full transcribe -> align -> (optionally) diarize + label. Caller holds the lock."""
    audio = whisperx.load_audio(str(path))          # 16 kHz mono via torchcodec
    result = CACHE.asr().transcribe(
        audio, batch_size=ARGS.batch_size, language=language
    )
    lang = result.get("language") or language or "en"
    align_model, align_meta = CACHE.align(lang)
    result = whisperx.align(
        result["segments"], align_model, align_meta, audio, DEVICE,
        return_char_alignments=False,
    )
    diar = None
    if diarize:
        # Absent flags mean "use transcriber-diarizer.py's own defaults".
        kwargs = {k: v for k, v in (("merge_sim", ARGS.merge_sim),
                                    ("min_speaker_seconds",
                                     ARGS.min_speaker_seconds)) if v is not None}
        diar = CACHE.diarizer().diarize(audio, speaker_labels=speaker_labels,
                                       **kwargs)
        assign_speakers(result, diar)
    return result, lang, diar


def _join_words(tokens):
    text = " ".join(str(t).strip() for t in tokens if str(t).strip())
    return re.sub(r"\s+", " ", text).strip()


def assign_speakers(result, diar):
    """Label whisperx's aligned words, then group them into utterances.

    Maximum time-overlap wins: a word that begins while the previous speaker is
    still finishing is exactly where "who was active at the midpoint" mislabels
    overlapping turns.  The nearest-segment fallback (within --assign-window)
    covers gaps the diarizer drops — breaths, laughter, page turns — and words
    that match nothing inherit the previous speaker rather than getting an
    "unknown" line, because a stray unlabelled utterance mid-turn reads to a
    human as a third participant.
    """
    segs = diar["segments"]
    words = sorted(result.get("word_segments") or [], key=lambda w: w.get("start", 0.0))
    if not segs or not words:
        return

    starts = [s["start"] for s in segs]
    prev = None
    for w in words:
        ws, we = float(w.get("start", 0.0)), float(w.get("end", 0.0))
        best, best_ov, near, near_d = None, 0.0, None, ARGS.assign_window + 1e-9
        i = bisect.bisect_right(starts, we) - 1
        while i >= 0:
            s = segs[i]
            if s["end"] < ws - ARGS.assign_window:
                break                                # older segments cannot match
            ov = min(we, s["end"]) - max(ws, s["start"])
            if ov > best_ov:
                best, best_ov = s["speaker"], ov
            center = 0.5 * (s["start"] + s["end"])
            d = ws - center if center < ws else (center - we if center > we else 0.0)
            if d < near_d:
                near, near_d = s["speaker"], d
            i -= 1
        prev = w["speaker"] = best if best_ov > 0 else \
            (near if near_d <= ARGS.assign_window else prev)

    utterances = []
    for w in words:
        start, end = float(w.get("start", 0.0)), float(w.get("end", 0.0))
        cur = utterances[-1] if utterances else None
        if cur is not None and cur["speaker"] == w.get("speaker") \
                and start - cur["end"] <= ARGS.utterance_gap:
            cur["end"] = max(cur["end"], end)
            cur["tokens"].append(w.get("word", ""))
        else:
            utterances.append({"start": start, "end": end,
                               "speaker": w.get("speaker"), "tokens": [w.get("word", "")]})

    result["utterances"] = [{"start": round(u["start"], 3), "end": round(u["end"], 3),
                             "speaker": u["speaker"],
                             "text": _join_words(u["tokens"])} for u in utterances]

    # Majority speaker per whisperx segment, so the ASR-side segmentation keeps
    # a label too (json consumers that walk `segments` rather than `utterances`).
    for seg in result.get("segments", []):
        ws, we = float(seg.get("start", 0.0)), float(seg.get("end", 0.0))
        votes = {}
        for w in words:
            if w.get("speaker") and ws <= float(w.get("start", 0.0)) <= we:
                votes[w["speaker"]] = votes.get(w["speaker"], 0) + 1
        if votes:
            seg["speaker"] = max(votes, key=lambda k: votes[k])


# ── Output rendering ───────────────────────────────────────────────────────

def fmt_ts_srt(seconds):
    ms = int(round(seconds * 1000))
    h, ms = divmod(ms, 3_600_000)
    m, ms = divmod(ms, 60_000)
    s, ms = divmod(ms, 1000)
    return f"{h:02d}:{m:02d}:{s:02d},{ms:03d}"


def fmt_ts_vtt(seconds):
    return fmt_ts_srt(seconds).replace(",", ".")


def display_items(result):
    """What the transcript body is made of: diarized utterances when we have them.

    Utterances beat whisperx's ASR segments for a labelled transcript because
    they follow turn boundaries instead of whisper's own chunking — see
    `assign_speakers`.
    """
    if result.get("utterances"):
        return result["utterances"]
    return [{"start": s.get("start", 0.0), "end": s.get("end", 0.0),
             "speaker": s.get("speaker"), "text": (s.get("text") or "").strip()}
            for s in result.get("segments", [])]


def transcript_text(result):
    """Speaker-labelled plain text: the `txt` format and the summarizer's input."""
    lines = []
    for u in display_items(result):
        spk = f"{u['speaker']}: " if u.get("speaker") else ""
        lines.append((spk + u["text"]).strip())
    return "\n".join(lines)


def render(result, fmt, diar=None, summary=None, summary_error=None,
           saved=None, headers=None):
    items = display_items(result)
    tail = None
    if summary is not None:
        tail = summary
    elif summary_error:
        tail = f"*(summary failed: {summary_error})*"

    if fmt == "json":
        payload = {
            "language": result.get("language"),
            "segments": [
                {
                    "start": round(s.get("start", 0.0), 3),
                    "end": round(s.get("end", 0.0), 3),
                    "speaker": s.get("speaker"),
                    "text": s.get("text", "").strip(),
                }
                for s in result.get("segments", [])
            ],
        }
        if result.get("utterances"):
            payload["utterances"] = items
        if diar:
            payload["speakers"] = diar.get("speakers")
            payload["diarization"] = {"model": SORTFORMER_NAME,
                                      "segments": diar["segments"],
                                      "slot_similarity": diar["similarity"],
                                      "merged_slots": diar["merges"],
                                      "ignored_slots": diar.get("ignored_slots")}
        if summary is not None:
            payload["summary_markdown"] = summary
        if summary_error:
            payload["summary_error"] = summary_error
        if saved:
            payload["saved"] = saved
        return JSONResponse(payload, headers=headers)

    lines = []
    if fmt == "srt":
        for i, u in enumerate(items, 1):
            spk = f"[{u['speaker']}] " if u.get("speaker") else ""
            lines += [str(i),
                      f"{fmt_ts_srt(u['start'])} --> {fmt_ts_srt(u['end'])}",
                      spk + u["text"],
                      ""]
    elif fmt == "vtt":
        lines += ["WEBVTT", ""]
        for u in items:
            spk = f"<v {u['speaker']}>" if u.get("speaker") else ""
            lines += [f"{fmt_ts_vtt(u['start'])} --> {fmt_ts_vtt(u['end'])}",
                      spk + u["text"],
                      ""]
    else:                                        # txt
        lines = [ln for ln in transcript_text(result).split("\n") if ln]

    if tail:
        lines += ["", "---", "", tail, ""]
    return PlainTextResponse("\n".join(lines), headers=headers)


# ── FastAPI app ────────────────────────────────────────────────────────────

app = FastAPI(title="peteches transcriber", docs_url=None, redoc_url=None)

INDEX_HTML = """<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>transcriber — WhisperX + diarization</title>
<style>
 :root { color-scheme: dark; }
 body { font-family: system-ui, sans-serif; background:#14161a; color:#e8e6e3;
        max-width: 860px; margin: 2rem auto; padding: 0 1rem; }
 h1 { font-size: 1.3rem; } h1 small { color:#9aa0a6; font-weight:normal; }
 fieldset { border:1px solid #2c3038; border-radius:8px; margin-bottom:1rem; }
 label { display:inline-block; margin:.4rem .8rem .4rem 0; }
 select, input[type=text] { background:#1d2026; color:#e8e6e3; border:1px solid #2c3038;
        border-radius:4px; padding:.3rem .5rem; }
 button { background:#3b82f6; border:0; border-radius:6px; color:#fff;
        padding:.55rem 1.2rem; font-size:1rem; cursor:pointer; }
 button:disabled { background:#33415c; cursor:wait; }
 pre { background:#111318; border:1px solid #2c3038; border-radius:8px;
       padding:1rem; white-space:pre-wrap; max-height:60vh; overflow:auto; }
 .hint { color:#9aa0a6; font-size:.85rem; }
 .err { color:#f87171; }
</style></head><body>
<h1>transcriber <small>WhisperX large-v3 on the 4090 — SortFormer speaker diarization, strata minutes</small></h1>
<form id="f">
  <fieldset><legend>Audio</legend>
    <label>File <input type="file" name="file" id="file" accept="audio/*,video/*" required></label>
    <label>Language <input type="text" name="language" id="language" placeholder="auto" size="5"></label>
    <label>Diarize <input type="checkbox" name="diarize" id="diarize" checked></label>
    <label>Summarize <input type="checkbox" name="summarize" id="summarize" checked></label>
    <label>Name the speakers <input type="text" name="speaker_labels" id="labels"
           placeholder="Gareth, Pete (order of first speech)" size="28"></label>
  </fieldset>
  <fieldset><legend>Output</legend>
    <label>Format
      <select name="format" id="format">
        <option value="txt" selected>txt (SPEAKER: line)</option>
        <option value="srt">srt</option>
        <option value="vtt">vtt</option>
        <option value="json">json</option>
      </select></label>
    <button id="go" type="submit">Transcribe</button>
    <span class="hint">first request loads the models (~15-30 s); later ones are fast.
      idle-unloaded after IDLE minutes.</span>
  </fieldset>
</form>
<div id="status" class="hint"></div>
<pre id="out" hidden></pre>
<script>
const f = document.getElementById('f'), out = document.getElementById('out'),
      st = document.getElementById('status'), go = document.getElementById('go');
f.addEventListener('submit', async (e) => {
  e.preventDefault();
  const fd = new FormData();
  fd.append('file', document.getElementById('file').files[0]);
  fd.append('format', document.getElementById('format').value);
  fd.append('diarize', document.getElementById('diarize').checked);
  fd.append('summarize', document.getElementById('summarize').checked);
  const labels = document.getElementById('labels').value.trim();
  if (labels) fd.append('speaker_labels', labels);
  const lang = document.getElementById('language').value.trim();
  if (lang) fd.append('language', lang);
  go.disabled = true; out.hidden = true; st.textContent = 'transcribing… (large files can take minutes)';
  const t0 = performance.now();
  try {
    const r = await fetch('/transcribe', { method: 'POST', body: fd });
    const body = await r.text();
    if (!r.ok) { st.innerHTML = '<span class="err">' + r.status + ' ' +
      body.replace(/[<>]/g, c => ({'<':'&lt;','>':'&gt;'}[c])) + '</span>'; return; }
    st.textContent = 'done in ' + ((performance.now()-t0)/1000).toFixed(1) + 's';
    try { out.textContent = JSON.stringify(JSON.parse(body), null, 2); }
    catch { out.textContent = body; }
    out.hidden = false;
  } catch (err) { st.innerHTML = '<span class="err">' + err + '</span>'; }
  finally { go.disabled = false; }
});
</script>
<p class="hint">API: <code>curl -F file=@audio.wav -F format=srt https://HOST/transcribe</code>
— <a href="/health">/health</a></p>
</body></html>
"""


@app.get("/", response_class=HTMLResponse)
async def index():
    return Response(
        content=INDEX_HTML.replace("IDLE", str(int(ARGS.idle_unload_minutes))),
        media_type="text/html",
    )


@app.get("/health")
async def health():
    return {
        "status": "ok",
        "device": DEVICE,
        "model_size": ARGS.model_size,
        "models_resident": CACHE.loaded(),
        # Diarization needs no secret: the checkpoint is ungated and either
        # already on disk or fetchable into the path below.
        "diarization_backend": "nvidia-sortformer-4spk-v2.1 (onnx)",
        "diarization_available": sortformer_fetchable(),
        "diarization_model_path": sortformer_model_path(),
        "diarization_resident": CACHE.diarizer_loaded(),
        "summarizer_url": ARGS.summarizer_base_url,
        "summary_dir": ARGS.summary_dir,
    }


def save_outputs(result, summary, diar, lang, stem):
    """Write the labelled transcript (+ minutes) under --summary-dir.

    Names are `<upload-stem>-<stamp>-transcript.txt' / `-minutes.md', so a
    recurring meeting's files sort chronologically by name.  The provenance
    comment in the minutes records the diarizer and who was on it, because a
    label like SPEAKER_01 is only meaningful next to its evidence.
    """
    os.makedirs(ARGS.summary_dir, exist_ok=True)
    # The state dir belongs to the service user and defaults to 0700, which
    # would leave the saved minutes readable only by root on that box.  Minutes
    # are meant to be collected (scp/git), so make the drop-off point traversable
    # and the files world-readable; contents stay inside a tailnet-only VM.
    try:
        os.chmod(ARGS.summary_dir, 0o755)
    except OSError:
        pass
    stamp = time.strftime("%Y%m%d-%H%M%S")
    base = re.sub(r"[^A-Za-z0-9._-]+", "-", stem).strip("-") or "meeting"
    paths = {}

    txt = Path(ARGS.summary_dir) / f"{base}-{stamp}-transcript.txt"
    txt.write_text(transcript_text(result) + "\n", encoding="utf-8")
    os.chmod(txt, 0o644)
    paths["transcript"] = str(txt)

    if summary is not None:
        who = ""
        if diar:
            who = "\n<!-- speakers: " + ", ".join(
                f"{s['label']} {s['seconds']:.0f}s" for s in diar["speakers"]) + " -->"
        md = Path(ARGS.summary_dir) / f"{base}-{stamp}-minutes.md"
        md.write_text(f"<!-- transcript: {txt} | language={lang} "
                      f"| diarizer={SORTFORMER_NAME}{who} -->\n\n"
                      f"{summary}\n", encoding="utf-8")
        os.chmod(md, 0o644)
        paths["summary"] = str(md)
    return paths


@app.post("/transcribe")
async def transcribe(
    file: UploadFile = File(...),
    format: str = Form("json"),
    language: str = Form(None),
    diarize: bool = Form(True),
    summarize: bool = Form(False),
    save: bool = Form(False),
    speaker_labels: str = Form(None),
):
    if format not in ("json", "srt", "vtt", "txt"):
        raise HTTPException(400, f"format must be json|srt|vtt|txt, got {format!r}")
    if DEVICE != "cuda":
        raise HTTPException(503, "no CUDA device visible to the transcriber")
    labels = [x.strip() for x in (speaker_labels or "").split(",") if x.strip()]

    suffix = Path(file.filename or "audio").suffix or ".audio"
    tmp = UPLOADS / f"{os.getpid()}-{threading.get_ident()}-{time.time_ns()}{suffix}"
    size = 0
    limit = ARGS.max_upload_mb * 1024 * 1024
    try:
        with open(tmp, "wb") as fh:
            while chunk := await file.read(1024 * 1024):
                size += len(chunk)
                if size > limit:
                    raise HTTPException(413, f"upload exceeds {ARGS.max_upload_mb} MB limit")
                fh.write(chunk)

        started = time.time()
        # CACHE._lock serialises GPU work across requests (see module notes).
        with CACHE._lock:
            CACHE.touch()
            try:
                result, lang, diar = run_pipeline(tmp, language or None, diarize,
                                                  labels or None)
            except Exception as exc:                     # decode/OOM/etc.
                raise HTTPException(422, f"transcription failed: {exc!r}")
            CACHE.touch()
        gpu_time = time.time() - started

        # Minutes come after the GPU lock is released: an LLM call must not sit
        # in front of the transcription queue, and task allocation needs the
        # speaker labels to be able to name owners at all.
        summary = summary_error = None
        if summarize:
            if diar is None:
                summary_error = ("summarize needs diarize=true — task allocation "
                                 "cannot attribute owners to an unlabelled "
                                 "transcript")
            else:
                try:
                    summary = summarize_transcript(transcript_text(result))
                except Exception as exc:
                    summary_error = repr(exc)

        saved = {}
        if save:
            stem = Path(file.filename or "audio").stem
            try:
                saved = save_outputs(result, summary, diar, lang, stem)
            except OSError as exc:
                raise HTTPException(500, f"could not save outputs: {exc!r}")

        headers = {f"X-{k.title()}-Path": v for k, v in saved.items()}
        print(f"[transcriber] {file.filename}: {size/1e6:.1f} MB in "
              f"{gpu_time:.1f}s (lang={lang}, diarize={diarize}, "
              f"summarize={bool(summary)}, saved={list(saved) or '-'})",
              flush=True)
        return render(result, format, diar=diar, summary=summary,
                      summary_error=summary_error, saved=saved or None,
                      headers=headers or None)
    finally:
        tmp.unlink(missing_ok=True)


def main():
    print(f"[transcriber] device={DEVICE} model={ARGS.model_size} "
          f"compute={ARGS.compute_type} port={ARGS.port} "
          f"diarizer={sortformer_model_path()} "
          f"(present={'yes' if Path(sortformer_model_path()).is_file() else 'will-download'}) "
          f"summarizer={ARGS.summarizer_base_url} summary_dir={ARGS.summary_dir}",
          flush=True)
    uvicorn.run(app, host=ARGS.host, port=ARGS.port, log_level="info")


if __name__ == "__main__":
    main()
