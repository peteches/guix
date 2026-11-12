#!/usr/bin/env python3
"""transcriber — WhisperX transcription + speaker diarization API.

Single-file FastAPI app served by (peteches services transcriber) inside the
uv-managed venv on the comfyui VM's RTX 4090.  Deliberately self-contained
(no package structure): the Guix service references this file directly via
local-file, so editing it here + redeploying is the whole release process.

Endpoints:
  GET  /            minimal upload UI (file -> speaker-labelled transcript)
  POST /transcribe  multipart `file` + optional `format` (json|srt|vtt|txt),
                    `language` (ISO code, default auto-detect), `diarize`
                    (bool, default true), `model` (whisper size, default from
                    CLI).  Returns the transcript in the requested format.
  GET  /health      liveness + whether models are resident + diarization
                    availability (HF token present and readable)

Design notes:
  - Models are LAZY-LOADED on first request and UNLOADED after an idle
    timeout (default 10 min): the 4090 is shared with ComfyUI (~13.5GB) and
    AllTalk/XTTS (~2.2GB), so this service must not squat ~4GB of VRAM
    between transcriptions.  Idle cost is just the Python process.
  - One GPU worker at a time: a module-level lock serialises requests
    (faster-whisper batching already saturates the card; queueing beats
    OOMing when ComfyUI is mid-workflow).
  - Diarization needs a HuggingFace token whose account has accepted the
    terms of pyannote/speaker-diarization-community-1 (whisperx's default
    pipeline).  The token is read from --hf-token-file at request time (not
    startup) so dropping the secret in place needs no restart.  Without it,
    diarize=true fails with a clear 503 and diarize=false still works.
  - SRT/VTT are rendered here (with [SPEAKER_xx] prefixes) rather than via
    whisperx's SubtitlesProcessor, to avoid its nltk punkt dependency.
"""

import argparse
import html
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

parser = argparse.ArgumentParser(description="WhisperX transcriber service")
parser.add_argument("--port", type=int, default=8787)
parser.add_argument("--host", default="0.0.0.0")
parser.add_argument("--model-size", default="large-v3")
parser.add_argument("--compute-type", default="float16")
parser.add_argument("--batch-size", type=int, default=4)
parser.add_argument("--idle-unload-minutes", type=float, default=10.0)
parser.add_argument("--max-upload-mb", type=int, default=500)
parser.add_argument("--hf-token-file", default="/run/secrets/hf-token")
parser.add_argument("--uploads-dir", default="/var/lib/transcriber/uploads")
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
        self._diarize = None
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

    def diarize(self, token):
        if self._diarize is None:
            self._diarize = whisperx.DiarizationPipeline(
                use_auth_token=token, device=DEVICE
            )
        return self._diarize

    # -- lifecycle -----------------------------------------------------------
    def touch(self):
        self._last_use = time.monotonic()
        if self._timer is None and ARGS.idle_unload_minutes > 0:
            self._timer = threading.Timer(
                ARGS.idle_unload_minutes * 60, self._unload_if_idle
            )
            self._timer.daemon = True
            self._timer.start()

    def _unload_if_idle(self):
        with self._lock:
            self._timer = None
            if time.monotonic() - self._last_use >= ARGS.idle_unload_minutes * 60:
                self._asr = None
                self._align = {}
                self._diarize = None
                if DEVICE == "cuda":
                    torch.cuda.empty_cache()
                print("[transcriber] idle timeout: models unloaded, VRAM freed",
                      flush=True)

    def loaded(self):
        return self._asr is not None


CACHE = ModelCache()


def read_hf_token():
    """Read the HF token at request time; None when absent/unreadable."""
    try:
        tok = Path(ARGS.hf_token_file).read_text().strip()
        return tok or None
    except OSError:
        return None


# ── Pipeline ───────────────────────────────────────────────────────────────

def run_pipeline(path, language, diarize):
    """Full transcribe -> align -> (optionally) diarize. Caller holds the lock."""
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
    if diarize:
        token = read_hf_token()
        if token is None:
            raise DiarizationUnavailable()
        dia = CACHE.diarize(token)(audio)
        result = whisperx.assign_word_speakers(dia, result)
    return result, lang


class DiarizationUnavailable(Exception):
    pass


# ── Output rendering ───────────────────────────────────────────────────────

def fmt_ts_srt(seconds):
    ms = int(round(seconds * 1000))
    h, ms = divmod(ms, 3_600_000)
    m, ms = divmod(ms, 60_000)
    s, ms = divmod(ms, 1000)
    return f"{h:02d}:{m:02d}:{s:02d},{ms:03d}"


def fmt_ts_vtt(seconds):
    return fmt_ts_srt(seconds).replace(",", ".")


def render(result, fmt):
    segments = result.get("segments", [])
    if fmt == "json":
        return JSONResponse({
            "language": result.get("language"),
            "segments": [
                {
                    "start": round(s.get("start", 0.0), 3),
                    "end": round(s.get("end", 0.0), 3),
                    "speaker": s.get("speaker"),
                    "text": s.get("text", "").strip(),
                }
                for s in segments
            ],
        })
    lines = []
    if fmt == "srt":
        for i, s in enumerate(segments, 1):
            spk = f"[{s['speaker']}] " if s.get("speaker") else ""
            lines += [str(i),
                      f"{fmt_ts_srt(s.get('start', 0))} --> {fmt_ts_srt(s.get('end', 0))}",
                      spk + s.get("text", "").strip(),
                      ""]
        return PlainTextResponse("\n".join(lines))
    if fmt == "vtt":
        lines.append("WEBVTT")
        lines.append("")
        for s in segments:
            spk = f"<v {s['speaker']}>" if s.get("speaker") else ""
            lines += [f"{fmt_ts_vtt(s.get('start', 0))} --> {fmt_ts_vtt(s.get('end', 0))}",
                      spk + s.get("text", "").strip(),
                      ""]
        return PlainTextResponse("\n".join(lines))
    # txt: one line per segment, speaker-prefixed when diarized
    for s in segments:
        spk = f"{s['speaker']}: " if s.get("speaker") else ""
        lines.append(spk + s.get("text", "").strip())
    return PlainTextResponse("\n".join(lines))


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
<h1>transcriber <small>WhisperX large-v3 on the 4090 — speaker diarization via pyannote</small></h1>
<form id="f">
  <fieldset><legend>Audio</legend>
    <label>File <input type="file" name="file" id="file" accept="audio/*,video/*" required></label>
    <label>Language <input type="text" name="language" id="language" placeholder="auto" size="5"></label>
    <label>Diarize <input type="checkbox" name="diarize" id="diarize" checked></label>
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
        "diarization_available": read_hf_token() is not None,
    }


@app.post("/transcribe")
async def transcribe(
    file: UploadFile = File(...),
    format: str = Form("json"),
    language: str = Form(None),
    diarize: bool = Form(True),
):
    if format not in ("json", "srt", "vtt", "txt"):
        raise HTTPException(400, f"format must be json|srt|vtt|txt, got {format!r}")
    if DEVICE != "cuda":
        raise HTTPException(503, "no CUDA device visible to the transcriber")
    if diarize and read_hf_token() is None:
        raise HTTPException(
            503,
            "diarization needs a HuggingFace token at "
            f"{ARGS.hf_token_file} (account must have accepted the terms of "
            "pyannote/speaker-diarization-community-1). Retry with diarize=false "
            "for a plain transcript.",
        )

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
                result, lang = run_pipeline(tmp, language or None, diarize)
            except DiarizationUnavailable:
                raise HTTPException(503, "HF token vanished mid-request")
            except Exception as exc:                     # decode/OOM/etc.
                raise HTTPException(422, f"transcription failed: {exc!r}")
            CACHE.touch()
        print(f"[transcriber] {file.filename}: {size/1e6:.1f} MB in "
              f"{time.time()-started:.1f}s (lang={lang}, diarize={diarize})",
              flush=True)
        return render(result, format)
    finally:
        tmp.unlink(missing_ok=True)


def main():
    print(f"[transcriber] device={DEVICE} model={ARGS.model_size} "
          f"compute={ARGS.compute_type} port={ARGS.port} "
          f"hf_token={'present' if read_hf_token() else 'ABSENT (diarization disabled)'}",
          flush=True)
    uvicorn.run(app, host=ARGS.host, port=ARGS.port, log_level="info")


if __name__ == "__main__":
    main()
