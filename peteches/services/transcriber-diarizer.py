#!/usr/bin/env python3
"""Speaker diarization for the transcriber service: NVIDIA SortFormer v2.1
run through onnxruntime + numpy — no NeMo, no pyannote, no HuggingFace token.

Why this exists
---------------
whisperx's own diarization path is pyannote.audio, whose community-1 models are
`gated=auto` on HuggingFace: an anonymous weight fetch 401s, so a diarized
transcript needs the operator to accept the model's terms and ship an HF token
into the VM.  NVIDIA's `diar_streaming_sortformer_4spk-v2.1` is ungated under
the NVIDIA Open Model License, third parties (cgus, ooobo) publish plain ONNX
exports of it, and onnxruntime was already in the transcriber venv — so this
backend adds no pip pins, no secret and no license gate.

What this file is
-----------------
The exported graph is only half the model: given one chunk plus a speaker cache
and FIFO queue it returns per-frame speaker probabilities for
`[spkcache | fifo | chunk]` and the chunk's pre-encode embeddings.  All of
NeMo's *streaming state machine* — FIFO popping, arrival-order speaker-cache
compression by importance score, silence-profile bookkeeping — lives in the
caller.  This module is a numpy port of that state machine.

References (both permissive: MIT / Apache-2.0), cited below as
`sortformer.rs` / `audio.rs` for altunenes/parakeet-rs (MIT, an ONNX-level Rust
implementation of this model family) and `modules.py` / `diar_models.py` for
NVIDIA NeMo's `nemo/collections/asr/modules/sortformer_modules.py` and
`.../models/sortformer_diar_models.py` (Apache-2.0, the semantics the Rust port
reproduces; its `forward_for_export` defines the ONNX IO contract).

Graph contract — verified empirically against this export on comfyui:
  IN  chunk                float32 [B, mel_frames, 128]   10 ms mel frames
      chunk_lengths        int64   [B]                     valid mel_frames
      spkcache             float32 [B, 188, 512]           arrival-order cache
      spkcache_lengths     int64   [B]
      fifo                 float32 [B, 40, 512]            recent pre-encode embs
      fifo_lengths         int64   [B]
  OUT spkcache_fifo_chunk_preds float32 [B, 188+40+(mel_frames/8), 4] — sigmoid
      speaker activity for the packed region at 80 ms resolution; rows past the
      valid lengths are exactly zero.
      chunk_pre_encode_embs  float32 [B, mel_frames/8, 512]
      chunk_pre_encode_lengths int64 [B]

Speaker identity is stable over a whole file by construction: the cache is kept
in arrival order (NVIDIA's AOSC) and frames are never re-assigned.

Constants come from `model_config.yaml` inside NVIDIA's own
`diar_streaming_sortformer_4spk-v2.1.nemo`, which is authoritative for this
checkpoint, plus NVIDIA's "very high latency" offline streaming preset from the
model card (chunk 340 / right context 40 / fifo 40 / update period 300 / cache
188 — the row of the latency table that processes a whole file).
"""

import math
import os
import sys
import time

import numpy as np

# ── audio frontend (NeMo AudioToMelSpectrogramPreprocessor, normalize='NA') ─

SAMPLE_RATE = 16000          # preprocessor.sample_rate
N_FFT = 512                  # preprocessor.n_fft
WIN_LENGTH = 400             # preprocessor.window_size = 0.025 s @ 16 kHz
HOP_LENGTH = 160             # preprocessor.window_stride = 0.01 s @ 16 kHz
N_MELS = 128                 # preprocessor.features
PREEMPH = 0.97               # NeMo's default pre-emphasis
# NeMo's log guard (log_zero_guard_value = 2**-24).  Its dither is 1e-5 white
# noise; we skip the random term for determinism, which the guard makes
# harmless — parakeet-rs does the same (sortformer.rs:1390-1405).
LOG_ZERO_GUARD = 2.0 ** -24

# ── model / streaming constants ────────────────────────────────────────────

N_SPK = 4                    # sortformer_modules.num_spks
EMB_DIM = 512                # sortformer_modules.fc_d_model
SUBSAMPLING = 8              # encoder subsampling: 10 ms mel frames -> 80 ms frames
FRAME_SEC = 0.08             # one model frame

CHUNK_LEN = 340              # frames emitted per step (27.2 s of audio)
RIGHT_CONTEXT = 40           # lookahead frames: attended by the encoder, not emitted
FIFO_LEN = 40                # FIFO capacity in frames
SPKCACHE_LEN = 188           # speaker-cache capacity in frames
UPDATE_PERIOD = 300          # frames moved FIFO -> cache per update

# Left context.  NeMo prepends `chunk_left_context` (config: 1) encoder frames;
# parakeet-rs runs this export with none, and the graph's dynamic axes accept
# either.  We follow the ONNX consumer (no left-context frame): the FIFO's 40
# overlapping frames already provide history, so the difference is one 80 ms
# frame of context.  Keeping it at 0 makes our emitted-row arithmetic match.
LEFT_CONTEXT = 0

SIL_FRAMES_PER_SPK = 3       # sortformer_modules.spkcache_sil_frames_per_spk
PRED_SCORE_THRESHOLD = 0.25  # sortformer_modules.pred_score_threshold
SIL_THRESHOLD = 0.2          # a frame counts as silence when sum(preds) < this
STRONG_BOOST_RATE = 0.75     # sortformer_modules.strong_boost_rate
WEAK_BOOST_RATE = 1.5        # sortformer_modules.weak_boost_rate
MIN_POS_SCORES_RATE = 0.5    # sortformer_modules.min_pos_scores_rate
SCORES_BOOST_LATEST = 0.05   # favor frames appended since the last compression

# Post-hoc cluster merging, in cosine similarity of per-cluster mean pre-encode
# embeddings (measured on this checkpoint: two slots that turned out to be one
# speaker under different mic distances scored 0.985 against each other and
# 0.76-0.77 against the other participant — see the note in
# transcriber-server.py's module docstring).  AOS keeps a speaker in ONE slot,
# so an extra slot means either a real third voice or the same voice under a
# changed acoustic condition; merging only at a high threshold catches the
# latter without inventing identities.  0 disables merging.
DEFAULT_MERGE_SIM = 0.90

# Cluster embeddings are only needed to compute means, so they are thinned as
# they arrive (streaming subsampling with doubling) — bounded memory on files
# of any length instead of one float32[512] per active frame.
CLUSTER_EMB_CAP = 4096

# A slot that speaks less than this over the whole file is not a participant: it
# is one or two frames of laughter, a door, or the model hallucinating a fourth
# voice.  Reporting it as SPEAKER_03 would put a phantom person in the transcript
# and in the minutes; those words get the nearest-segment/inheritance treatment
# instead.  Slots are dropped only if at least one stays above the bar.
DEFAULT_MIN_SPEAKER_SECONDS = 8.0

# Binarization defaults.  NeMo's `diarize()` for this model is a plain 0.5
# threshold with no smoothing or duration filtering (parakeet-rs'
# DiarizationConfig::default, sortformer.rs:96-108 records that).  For meeting
# transcripts we smooth over 3 frames (240 ms) and drop sub-second blips: raw
# per-frame noise otherwise tears turns into fragments and makes the word ->
# speaker merge jitter.  All of it is exposed so a deployment can dial it back.
DEFAULT_ONSET = 0.5
DEFAULT_OFFSET = 0.5
DEFAULT_MEDIAN_WINDOW = 3    # in model frames; <= 1 disables smoothing
DEFAULT_MIN_DURATION_ON = 0.40
DEFAULT_MIN_DURATION_OFF = 0.24

MODEL_URL = ("https://huggingface.co/cgus/diar_streaming_sortformer_4spk-v2.1-onnx"
             "/resolve/main/diar_streaming_sortformer_4spk-v2.1.onnx")


def log(msg):
    print(f"[diarizer] {msg}", flush=True)


# ── frontend: 16 kHz samples -> (mel_frames, 128) log-mel ─────────────────

def _bf16_round(x):
    """Round float32 to nearest bfloat16 and back (round-half-to-even).

    The checkpoint stores the STFT window in bf16 and NeMo multiplies with
    those values, so our features match NeMo only if we do too (parakeet-rs'
    `to_bf16`, sortformer.rs:65-80).
    """
    u = np.asarray(x, dtype=np.float32).view(np.uint32)
    return (((u + 0x7FFF + ((u >> 16) & 1)) & 0xFFFF0000).astype(np.uint32)
            .view(np.float32))


def _slaney_hz_to_mel(hz):
    """librosa's Slaney mel scale (audio.rs:133-145)."""
    f_sp = 200.0 / 3.0
    min_log_hz, min_log_mel = 1000.0, 1000.0 / (200.0 / 3.0)
    log_step = 0.06875177742094912
    hz = np.asarray(hz, dtype=np.float64)
    # np.where evaluates both branches, so clip before the log: hz=0 (the first
    # mel-point edge) would otherwise raise a harmless divide-by-zero warning.
    return np.where(hz < min_log_hz, hz / f_sp,
                    min_log_mel + np.log(np.clip(hz, 1e-9, None) / min_log_hz) / log_step)


def _slaney_mel_to_hz(mel):
    f_sp = 200.0 / 3.0
    min_log_hz, min_log_mel = 1000.0, 1000.0 / (200.0 / 3.0)
    log_step = 0.06875177742094912
    mel = np.asarray(mel, dtype=np.float64)
    return np.where(mel < min_log_mel, mel * f_sp,
                    min_log_hz * np.exp((mel - min_log_mel) * log_step))


def mel_filterbank(n_fft=N_FFT, n_mels=N_MELS, sample_rate=SAMPLE_RATE):
    """Slaney mel filterbank, area-normalized (audio.rs:154-192).

    Built in float64 and cast to float32 exactly as the Rust port does — note
    that only the STFT window is bf16-rounded, not these filters.
    """
    freq_bins = n_fft // 2 + 1
    mmin = float(_slaney_hz_to_mel(0.0))
    mmax = float(_slaney_hz_to_mel(sample_rate / 2.0))
    mel_points = _slaney_mel_to_hz(
        mmin + (mmax - mmin) * np.arange(n_mels + 2, dtype=np.float64) / (n_mels + 1))
    fft_freqs = np.arange(freq_bins, dtype=np.float64) * sample_rate / n_fft
    fdiff = np.diff(mel_points)

    fb = np.maximum(0.0, np.minimum(
        (fft_freqs[None, :] - mel_points[:-2, None]) / fdiff[:-1, None],
        (mel_points[2:, None] - fft_freqs[None, :]) / fdiff[1:, None]))
    enorm = 2.0 / (mel_points[2:] - mel_points[:-2])            # Slaney norm
    return np.ascontiguousarray((fb * enorm[:, None]).astype(np.float32))


class MelFrontend:
    """NeMo's mel front end: pre-emphasis -> centered STFT -> mel -> log."""

    BLOCK = 8192          # frames per FFT block; bounds peak RAM on long files

    def __init__(self):
        # torch.hann_window(win_length, periodic=False) (divide by N-1), then
        # zero-padded to n_fft and CENTERED in the FFT buffer, bf16-rounded
        # (sortformer.rs:1332-1349).
        hann = 0.5 - 0.5 * np.cos(2 * np.pi
                                  * np.arange(WIN_LENGTH, dtype=np.float64)
                                  / (WIN_LENGTH - 1))
        off = (N_FFT - WIN_LENGTH) // 2
        self.window = np.zeros(N_FFT, dtype=np.float32)
        self.window[off:off + WIN_LENGTH] = _bf16_round(hann.astype(np.float32))
        self.mel = mel_filterbank()

    def n_frames(self, n_samples):
        """Frame count for N samples under center=True padding of n_fft//2."""
        return 1 + n_samples // HOP_LENGTH

    def __call__(self, samples):
        audio = np.ascontiguousarray(samples, dtype=np.float32)
        if audio.ndim != 1:
            raise ValueError("expected 1-D mono audio")

        # y[0] = x[0]; y[n] = x[n] - 0.97 * x[n-1]   (audio.rs:49-62)
        pre = np.empty_like(audio)
        if audio.size:
            pre[0] = audio[0]
            pre[1:] = audio[1:] - PREEMPH * audio[:-1]

        pad = N_FFT // 2
        padded = np.concatenate([np.zeros(pad, dtype=np.float32), pre,
                                 np.zeros(pad, dtype=np.float32)])
        n_frames = self.n_frames(audio.size)
        out = np.empty((n_frames, N_MELS), dtype=np.float32)

        for begin in range(0, n_frames, self.BLOCK):
            end = min(begin + self.BLOCK, n_frames)
            frames = padded[np.arange(begin, end)[:, None] * HOP_LENGTH
                            + np.arange(N_FFT)[None, :]] * self.window[None, :]
            spec = np.fft.rfft(frames, n=N_FFT, axis=1)
            power = (spec.real ** 2 + spec.imag ** 2).astype(np.float32)   # mag_power=2.0
            out[begin:end] = np.log((power @ self.mel.T) + LOG_ZERO_GUARD,
                                    dtype=np.float32)
        return out


# ── streaming state ────────────────────────────────────────────────────────

class SortformerState:
    """Speaker cache, FIFO queue and running silence profile (batch = 1)."""

    __slots__ = ("cache", "cache_preds", "fifo", "fifo_preds",
                 "mean_sil", "n_sil", "compressed")

    def __init__(self):
        self.cache = np.zeros((0, EMB_DIM), dtype=np.float32)
        self.cache_preds = np.zeros((0, N_SPK), dtype=np.float32)
        self.fifo = np.zeros((0, EMB_DIM), dtype=np.float32)
        self.fifo_preds = np.zeros((0, N_SPK), dtype=np.float32)
        self.mean_sil = np.zeros(EMB_DIM, dtype=np.float32)
        self.n_sil = 0
        self.compressed = False


# ── the diarizer ───────────────────────────────────────────────────────────

class SortformerDiarizer:
    """Offline diarization of a whole recording at 80 ms frame resolution."""

    def __init__(self, model_path, providers=None, intra_op_threads=None,
                 onset=DEFAULT_ONSET, offset=DEFAULT_OFFSET,
                 median_window=DEFAULT_MEDIAN_WINDOW,
                 min_duration_on=DEFAULT_MIN_DURATION_ON,
                 min_duration_off=DEFAULT_MIN_DURATION_OFF):
        import onnxruntime as ort

        opts = ort.SessionOptions()
        opts.log_severity_level = 3                  # hide ORT's chatty startup
        if intra_op_threads:
            opts.intra_op_num_threads = int(intra_op_threads)
            opts.inter_op_num_threads = 1
        self.model_path = model_path
        self.providers = list(providers or ["CPUExecutionProvider"])
        self.session = ort.InferenceSession(model_path, sess_options=opts,
                                            providers=self.providers)
        self.frontend = MelFrontend()
        self.onset, self.offset = float(onset), float(offset)
        self.median_window = int(median_window)
        self.min_duration_on = float(min_duration_on)
        self.min_duration_off = float(min_duration_off)

    # -- one streaming step -------------------------------------------------
    def _step(self, state, window):
        """Run the graph on one chunk of mel frames.

        Returns (chunk_preds, chunk_embs, packed_preds).  `window` is a real
        slice of features — NeMo never zero-pads the tail of a stream; raggedness
        is carried by the *_lengths inputs (`streaming_feat_loader`,
        modules.py:270-318).  Buffers are passed at full capacity with their
        lengths marking the valid prefix, which is what the export expects.
        """
        mel_frames = int(window.shape[0])
        sl, fl = state.cache.shape[0], state.fifo.shape[0]

        feats = np.zeros((1, mel_frames, N_MELS), dtype=np.float32)
        feats[0] = window
        cache_in = np.zeros((1, SPKCACHE_LEN, EMB_DIM), dtype=np.float32)
        cache_in[0, :sl] = state.cache
        fifo_in = np.zeros((1, FIFO_LEN, EMB_DIM), dtype=np.float32)
        fifo_in[0, :fl] = state.fifo

        outs = self.session.run(
            None,
            {"chunk": feats,
             "chunk_lengths": np.array([mel_frames], dtype=np.int64),
             "spkcache": cache_in,
             "spkcache_lengths": np.array([sl], dtype=np.int64),
             "fifo": fifo_in,
             "fifo_lengths": np.array([fl], dtype=np.int64)})
        preds, embs, pre_lens = outs

        # The graph's own count of valid chunk frames is authoritative for how
        # much of the packed prediction region belongs to this chunk.
        n_enc = min(int(pre_lens[0]), int(preds.shape[1] - sl - fl))
        # Only the first CHUNK_LEN of them are emitted audio; the trailing
        # right-context frames were fed for lookahead and are dropped (NeMo's
        # max_chunk_len = physical - lc - rc, modules.py:890-915).
        n_keep = min(CHUNK_LEN, n_enc)

        packed = preds[0]
        begin = sl + fl + LEFT_CONTEXT
        return packed[begin:begin + n_keep], embs[0, :n_keep], packed

    # -- FIFO -> cache bookkeeping -----------------------------------------
    @staticmethod
    def _pop_lengths(fifo_len, keep):
        """NeMo's `_compute_async_fifo_pop_lengths` specialized to batch=1."""
        combined = fifo_len + keep
        if combined <= FIFO_LEN:
            return 0, combined
        pop = min(combined, max(UPDATE_PERIOD, combined - FIFO_LEN))
        return pop, combined - pop

    def _update_state(self, state, chunk_preds, chunk_embs, packed_preds):
        """Move this step's frames through the FIFO into the speaker cache.

        NeMo does this with batched torch.gather acrobatics; with batch=1 the
        packed index arithmetic collapses to slicing (`_update_async_fifo`,
        `_update_async_spkcache`).
        """
        keep = int(chunk_embs.shape[0])
        sl = state.cache.shape[0]

        # Treat [old FIFO | this chunk] as one logical sequence (NeMo's phrasing).
        all_embs = np.concatenate([state.fifo, chunk_embs]) if state.fifo.shape[0] \
            else chunk_embs
        all_preds = np.concatenate([state.fifo_preds, chunk_preds]) \
            if len(state.fifo_preds) else chunk_preds

        pop, new_fifo = self._pop_lengths(state.fifo.shape[0], keep)
        if pop <= 0:
            state.fifo, state.fifo_preds = all_embs, all_preds
            return

        popped_embs, popped_preds = all_embs[:pop], all_preds[:pop]
        # The retained FIFO is the *newest* frames of the logical sequence.
        state.fifo = all_embs[all_embs.shape[0] - new_fifo:] if new_fifo \
            else np.zeros((0, EMB_DIM), "float32")
        state.fifo_preds = all_preds[all_preds.shape[0] - new_fifo:] if new_fifo \
            else np.zeros((0, N_SPK), "float32")

        # Running silence profile over the frames leaving the FIFO: cache slots
        # that get disabled are filled with this mean embedding.
        is_sil = popped_preds.sum(axis=1) < SIL_THRESHOLD
        if is_sil.any():
            n_new = state.n_sil + int(is_sil.sum())
            state.mean_sil = ((state.mean_sil * state.n_sil
                               + popped_embs[is_sil].sum(axis=0)) / max(n_new, 1)
                              ).astype(np.float32)
            state.n_sil = n_new

        # Until the first compression the cache has no stored importance; take
        # its predictions from this step's packed output (NeMo's
        # candidate_old_preds on `first_compression`, parakeet-rs'
        # initial_cache_preds at sortformer.rs:930-940).
        old_preds = state.cache_preds if state.compressed else packed_preds[:sl]

        state.cache = np.concatenate([state.cache, popped_embs])
        state.cache_preds = np.concatenate([old_preds, popped_preds])
        if state.cache.shape[0] > SPKCACHE_LEN:
            self._compress_spkcache(state)
            state.compressed = True

    def _compress_spkcache(self, state):
        """Keep the SPKCACHE_LEN most informative frames (NeMo's AOSC rule).

        Selection happens over (speaker, frame) *pairs* in a speaker-major
        flattened score matrix, so after sorting the chosen indices ascending
        the cache is ordered by speaker and, within a speaker, in original time
        order — exactly what NeMo documents for `_compress_spkcache`
        ("Embeddings are ordered by speakers. Within each speaker, original
        order of frames is kept").  A frame may be kept once per speaker.
        """
        n_frames = state.cache.shape[0]
        per_spk = SPKCACHE_LEN // N_SPK                       # 47
        if per_spk <= SIL_FRAMES_PER_SPK:                     # nothing to choose
            state.cache = state.cache[:SPKCACHE_LEN]
            state.cache_preds = state.cache_preds[:SPKCACHE_LEN]
            return

        len_per_spk = per_spk - SIL_FRAMES_PER_SPK            # 44
        strong = int(len_per_spk * STRONG_BOOST_RATE)         # floor -> 33
        weak = int(len_per_spk * WEAK_BOOST_RATE)             # 66
        min_pos = int(len_per_spk * MIN_POS_SCORES_RATE)      # 22

        preds = state.cache_preds.astype(np.float64)
        p = np.clip(preds, PRED_SCORE_THRESHOLD, 1.0)
        q = np.clip(1.0 - preds, PRED_SCORE_THRESHOLD, 1.0)
        # High where this speaker is confidently the only one active:
        # log p - log(1-p) + sum_s log(1-p_s) - log 0.5   (NeMo's formula).
        scores = np.log(p) - np.log(q) + np.log(q).sum(axis=1, keepdims=True) \
            - math.log(0.5)

        # Non-speech is worthless; overlapping speech is worth nothing to a
        # speaker that already has plenty of clean frames (`_disable_low_scores`).
        is_speech = preds > 0.5
        pos_count = (scores > 0).sum(axis=0)
        scores = np.where(is_speech, scores, -np.inf)
        scores = np.where((scores <= 0) & is_speech
                          & (pos_count[None, :] >= min_pos), -np.inf, scores)

        if SCORES_BOOST_LATEST > 0:                           # favor new frames
            scores[min(SPKCACHE_LEN, n_frames):] += SCORES_BOOST_LATEST

        # Boosting guarantees every speaker keeps some frames in the cache.
        shift = -math.log(0.5)                                # == log(2)
        for scale, k in ((2.0, strong), (1.0, weak)):
            if k <= 0:
                continue
            idx = np.argsort(-scores, axis=0, kind="stable")[:k]
            rows, cols = idx.ravel(), np.repeat(np.arange(N_SPK), k)
            vals = scores[rows, cols]
            scores[rows, cols] = np.where(np.isneginf(vals), vals, vals + scale * shift)

        # Reserve SIL_FRAMES_PER_SPK slots per speaker for silence: their
        # +inf scores make them win, and they resolve to the mean silence
        # embedding with zero predictions.
        sel = np.vstack([scores, np.full((SIL_FRAMES_PER_SPK, N_SPK), np.inf)])
        n_sel_rows = int(sel.shape[0])

        flat_scores = sel.T.reshape(-1)                       # index = s*n_rows + t
        order = np.argsort(-flat_scores, kind="stable")       # deterministic ties
        chosen = np.sort(order[:SPKCACHE_LEN])                # by (speaker, time)

        is_placeholder = (chosen % n_sel_rows) >= n_frames          # silence slots
        is_disabled = is_placeholder | (flat_scores[chosen] == -np.inf)
        # NeMo zeroes disabled selections before gathering so they cannot index
        # past the candidate array; the substitution happens below.
        frame = np.where(is_disabled, 0, chosen % n_sel_rows)

        embs = state.cache[frame]
        keep_preds = state.cache_preds[frame].copy()
        embs = np.where(is_disabled[:, None], state.mean_sil[None, :], embs)
        keep_preds = np.where(is_disabled[:, None], 0.0, keep_preds)

        state.cache = embs.astype(np.float32)
        state.cache_preds = keep_preds.astype(np.float32)

    # -- whole-file inference ----------------------------------------------
    def frame_probs(self, samples):
        """Speaker activity probabilities at model resolution: (frames, 4).

        Also returns per-cluster mean pre-encode embeddings, which is what
        `merge_clusters` compares: the frames SortFormer attributed to a speaker
        are exactly the evidence for *who* that speaker is.
        """
        t0 = time.time()
        feats = self.frontend(samples)
        n_mel = int(feats.shape[0])
        feed = (LEFT_CONTEXT + CHUNK_LEN + RIGHT_CONTEXT) * SUBSAMPLING

        state = SortformerState()
        chunks, steps = [], 0
        seen = [0] * N_SPK                       # frames attributed per cluster
        kept = [[] for _ in range(N_SPK)]        # reservoir of embedding samples
        # Reservoir sampling (Algorithm R) with a fixed seed: bounded memory,
        # but representative of the WHOLE file rather than of its first minutes.
        rngs = [np.random.default_rng(0x5EED + spk) for spk in range(N_SPK)]

        begin = 0
        while begin < n_mel:
            window = feats[begin:min(begin + feed, n_mel)]
            preds, embs, packed = self._step(state, window)
            chunks.append(preds)
            active = np.where((preds > 0.5).any(axis=1))[0]
            for row in active:
                spk = int(np.argmax(preds[row]))
                seen[spk] += 1
                pool = kept[spk]
                if len(pool) < CLUSTER_EMB_CAP:
                    pool.append(embs[row])
                else:
                    j = int(rngs[spk].integers(0, seen[spk]))
                    if j < CLUSTER_EMB_CAP:
                        pool[j] = embs[row]
            self._update_state(state, preds, embs, packed)
            begin += CHUNK_LEN * SUBSAMPLING
            steps += 1

        probs = np.concatenate(chunks) if chunks else np.zeros((0, N_SPK), "float32")
        means, counts = [], []
        for spk in range(N_SPK):
            if kept[spk]:
                m = np.stack(kept[spk]).mean(axis=0)
            else:
                m = state.mean_sil.copy()        # never active: silence profile
            means.append(m.astype(np.float64))
            counts.append(seen[spk])

        secs = samples.shape[0] / SAMPLE_RATE
        dt = time.time() - t0
        log(f"sortformer: {secs:.1f}s audio -> {probs.shape[0]} frames in "
            f"{dt:.1f}s ({steps} steps, RTF {dt / max(secs, 1e-9):.4f})")
        return probs, (means, counts)

    # -- predictions -> segments -------------------------------------------
    def _median_smooth(self, probs):
        w = self.median_window
        if w <= 1 or probs.shape[0] == 0:
            return probs
        half = w // 2
        padded = np.pad(probs, ((half, half), (0, 0)), mode="edge")
        stacked = np.stack([padded[i:i + probs.shape[0]] for i in range(w)])
        return np.median(stacked, axis=0).astype(np.float32)

    def segments_from_probs(self, probs):
        """Hysteresis binarization -> [(start_s, end_s, speaker), ...].

        Overlapping speech is kept (both speakers get a segment); deciding who
        "owns" a word is the transcript merge's job.  Segments are sorted by
        start time.
        """
        probs = self._median_smooth(probs)
        segs = []
        for spk in range(N_SPK):
            col = probs[:, spk] if probs.shape[0] else np.zeros(0, "float32")
            runs, on, start = [], False, 0
            for t in range(col.shape[0]):
                v = float(col[t])
                if not on:
                    if v > self.onset:
                        on, start = True, t
                elif v < self.offset:
                    runs.append((start, t))
                    on = False
            if on:
                runs.append((start, col.shape[0]))

            cand = [[s * FRAME_SEC, e * FRAME_SEC, spk] for s, e in runs
                    if (e - s) * FRAME_SEC >= self.min_duration_on]
            merged = []
            for seg in cand:
                if merged and seg[0] - merged[-1][1] < self.min_duration_off:
                    merged[-1][1] = max(merged[-1][1], seg[1])
                else:
                    merged.append(seg)
            segs.extend(merged)

        segs.sort(key=lambda s: (s[0], s[2]))
        return [(a, b, int(s)) for a, b, s in segs if b > a]

    # -- identity bookkeeping ----------------------------------------------
    @staticmethod
    def merge_slots(segments, means, counts, threshold):
        """Absorb slots that are measurably the same voice into one another.

        Greedy on similarity: take the highest-scoring pair above `threshold`,
        fold the quieter slot into the louder one (weighted by frames so the
        merged mean is still meaningful), and repeat until nothing qualifies —
        which handles the transitive case (A~B, B~C) deterministically.

        Returns (segments, alive, merges): segments are lists [start, end, slot]
        renumbered onto surviving slots, and `alive` maps slot -> [mean,
        frames] with means left unnormalized for correct weighted merging.
        """
        segs = [[s[0], s[1], s[2]] for s in segments]
        alive = {i: [np.asarray(means[i], dtype=np.float64), counts[i]]
                 for i in range(len(counts)) if counts[i] > 0}
        merges = []

        while len(alive) > 1:
            keys = sorted(alive)
            best_pair, best_sim = None, threshold
            for a in range(len(keys)):
                for b in range(a + 1, len(keys)):
                    i, j = keys[a], keys[b]
                    ma, mb = alive[i][0], alive[j][0]
                    den = np.linalg.norm(ma) * np.linalg.norm(mb)
                    sim = float(ma @ mb / den) if den > 1e-9 else 0.0
                    if sim >= best_sim:
                        best_pair, best_sim = (i, j), sim
            if best_pair is None:
                break

            i, j = best_pair
            keep, gone = (i, j) if alive[i][1] >= alive[j][1] else (j, i)
            mk, ck = alive[keep]
            mg, cg = alive[gone]
            alive[keep] = [mk * float(ck) + mg * float(cg), ck + cg]
            del alive[gone]
            for seg in segs:
                if seg[2] == gone:
                    seg[2] = keep
            merges.append({"from_slot": int(gone), "into_slot": int(keep),
                           "similarity": round(best_sim, 4)})

        return segs, alive, merges

    def diarize(self, samples, speaker_labels=None, merge_sim=DEFAULT_MERGE_SIM,
                min_speaker_seconds=DEFAULT_MIN_SPEAKER_SECONDS):
        """Diarize a whole recording; returns segments + who-is-who metadata.

        Slots are labelled SPEAKER_01, SPEAKER_02, ... in order of first speech,
        which is meaningful here because AOS assigns slots in arrival order.  An
        explicit `speaker_labels` list renames them in that same order (e.g.
        ["Gareth", "Pete"]) — the model cannot know names, only voices.
        """
        probs, (means, counts) = self.frame_probs(samples)
        segs = self.segments_from_probs(probs)

        alive = {i: [np.asarray(means[i], dtype=np.float64), counts[i]]
                 for i in range(len(counts)) if counts[i] > 0}
        merges = []
        if merge_sim and merge_sim > 0:
            segs, alive, merges = self.merge_slots(segs, means, counts,
                                                   float(merge_sim))

        # Drop slots too quiet to be a participant on a whole meeting.
        spoken = {}
        for a, b, slot in segs:
            spoken[slot] = spoken.get(slot, 0.0) + (b - a)
        ignored = []
        if min_speaker_seconds and min_speaker_seconds > 0:
            loud = [s for s, t in spoken.items() if t >= min_speaker_seconds]
            if loud:                           # never empty the transcript
                quiet = sorted((t, s) for s, t in spoken.items()
                               if t < min_speaker_seconds)
                ignored = [{"slot": int(s), "seconds": round(t, 2)} for t, s in quiet]
                drop = {s for _, s in quiet}
                segs = [s for s in segs if s[2] not in drop]

        first_seen = {}
        for a, b, slot in segs:
            first_seen.setdefault(slot, a)
        order = sorted(first_seen, key=lambda s: (first_seen[s], -alive[s][1]))
        names = list(speaker_labels or [])
        label_of = {slot: (names[i] if i < len(names) else f"SPEAKER_{i + 1:02d}")
                    for i, slot in enumerate(order)}

        unit = lambda slot: alive[slot][0] / max(np.linalg.norm(alive[slot][0]), 1e-9)
        similarity = [[round(float(unit(a) @ unit(b)), 4) for b in order]
                      for a in order]
        names = list(speaker_labels or [])
        label_of = {slot: (names[i] if i < len(names) else f"SPEAKER_{i + 1:02d}")
                    for i, slot in enumerate(order)}

        speakers = []
        for slot in order:
            own = [s for s in segs if s[2] == slot]
            speakers.append({"label": label_of[slot], "slot": int(slot),
                             "seconds": round(sum(s[1] - s[0] for s in own), 2),
                             "segments": len(own),
                             "active_frames": int(alive[slot][1]),
                             "first_seen": round(first_seen[slot], 2)})

        segments = [{"start": round(a, 3), "end": round(b, 3),
                     "speaker": label_of[slot], "slot": int(slot)}
                    for a, b, slot in sorted(segs, key=lambda s: (s[0], s[2]))]
        return {"segments": segments, "speakers": speakers,
                "ignored_slots": ignored, "similarity": similarity, "merges": merges,
                "frames": int(probs.shape[0])}


# ── CLI: transcriber-diarizer.py AUDIO [--model PATH] ─────────────────────

def load_audio(path, ffmpeg=None):
    """Decode anything ffmpeg understands to float32 mono at 16 kHz.

    The service finds ffmpeg on PATH inside its container; $FFMPEG or the system
    profile path are fallbacks for running this module by hand on the host.
    """
    import subprocess
    cands = [ffmpeg, os.environ.get("FFMPEG"),
             "/run/current-system/profile/bin/ffmpeg", "ffmpeg"]
    exe = next((c for c in cands
                if c and (not os.path.isabs(c) or os.access(c, os.X_OK))), "ffmpeg")
    raw = subprocess.run(
        [exe, "-v", "error", "-i", path, "-f", "f32le", "-acodec", "pcm_f32le",
         "-ac", "1", "-ar", str(SAMPLE_RATE), "-"],
        check=True, stdout=subprocess.PIPE).stdout
    return np.frombuffer(raw, dtype=np.float32)


def _fmt(t):
    m, s = divmod(float(t), 60)
    return f"{int(m):02d}:{s:05.2f}"


def main(argv=None):
    import argparse
    ap = argparse.ArgumentParser(description="SortFormer v2.1 diarization (ONNX)")
    ap.add_argument("audio")
    ap.add_argument("--model", default=os.path.expanduser("~/sf.onnx"))
    ap.add_argument("--threads", type=int, default=0, help="ort intra-op threads")
    ap.add_argument("--median-window", type=int, default=DEFAULT_MEDIAN_WINDOW)
    ap.add_argument("--min-duration-on", type=float, default=DEFAULT_MIN_DURATION_ON)
    ap.add_argument("--min-duration-off", type=float, default=DEFAULT_MIN_DURATION_OFF)
    ap.add_argument("--merge-sim", type=float, default=DEFAULT_MERGE_SIM,
                    help="merge slots whose mean embeddings are this similar "
                         "(<= 0 disables merging)")
    ap.add_argument("--min-speaker-seconds", type=float,
                    default=DEFAULT_MIN_SPEAKER_SECONDS,
                    help="ignore slots that speak less than this over the file")
    ap.add_argument("--labels", default="",
                    help="comma-separated names for the speakers, in order of "
                         "first speech (default SPEAKER_01, SPEAKER_02, ...)")
    a = ap.parse_args(argv)

    d = SortformerDiarizer(a.model, intra_op_threads=a.threads or None,
                           median_window=a.median_window,
                           min_duration_on=a.min_duration_on,
                           min_duration_off=a.min_duration_off)
    samples = load_audio(a.audio)
    out = d.diarize(samples, speaker_labels=[x for x in a.labels.split(",") if x],
                    merge_sim=a.merge_sim,
                    min_speaker_seconds=a.min_speaker_seconds)

    total = samples.shape[0] / SAMPLE_RATE
    covered = sum(s["end"] - s["start"] for s in out["segments"])
    for seg in out["segments"]:
        print(f"[{_fmt(seg['start'])}-{_fmt(seg['end'])}] {seg['speaker']}")
    print(f"-- {len(out['segments'])} segments over {total:.1f}s audio; speech "
          f"coverage {100*covered/max(total, 1e-9):.1f}% (counting overlaps twice)")
    for spk in out["speakers"]:
        print(f"   {spk['label']}: {spk['seconds']:.1f}s "
              f"({100*spk['seconds']/max(total, 1e-9):.1f}% of audio, "
              f"{spk['segments']} segments, first at {_fmt(spk['first_seen'])})")
    for m in out["merges"]:
        print(f"   merged slot {m['from_slot']} into {m['into_slot']} "
              f"(cosine {m['similarity']:.3f})")
    for ig in out.get("ignored_slots", []):
        print(f"   ignored slot {ig['slot']}: only {ig['seconds']:.1f}s of speech")
    if len(out["similarity"]) > 1:
        names = [s["label"] for s in out["speakers"]]
        print("   slot similarity (mean embeddings):")
        for i, row in enumerate(out["similarity"]):
            print(f"     {names[i]:>12s} " +
                  " ".join(f"{v:6.3f}" for v in row))


if __name__ == "__main__":
    main()
