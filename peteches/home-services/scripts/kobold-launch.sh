#!/usr/bin/env bash
set -euo pipefail

# ---- arg normalization -------------------------------------------------------
args=("$@")
# tolerate "gpu 4090 ..." as well as "4090 ..."
if [[ ${#args[@]} -ge 2 && "${args[0]}" == "gpu" ]]; then
  args=("${args[@]:1}")
fi
if [[ ${#args[@]} -lt 2 ]]; then
  echo "Usage: kobold-launch <cpu|GPU-regex> <model.gguf> [port=5001] [layers]" >&2
  exit 2
fi

GPU_REGEX="${args[0]}"
MODEL="${args[1]}"
PORT="${args[2]:-5001}"
LAYERS_OVERRIDE="${args[3]:-}"

# If user accidentally swapped args (MODEL not a file but PORT is), swap.
if [[ ! -f "$MODEL" && -f "${PORT:-}" ]]; then
  tmp="$MODEL"; MODEL="$PORT"; PORT="${args[3]:-5001}"; LAYERS_OVERRIDE="${args[4]:-}"
fi

# ---- locate koboldcpp (DEFAULT: Python) --------------------------------------
# Prefer python entrypoint, fall back to compiled binary if provided.
declare -a BIN
if [[ -n "${KOBOLDCPP_PY:-}" && -f "$KOBOLDCPP_PY" ]]; then
  BIN=("python3" "$KOBOLDCPP_PY")
elif [[ -f "$HOME/area_51/github.com/LostRuins/koboldcpp.git/koboldcpp.py" ]]; then
  BIN=("python3" "$HOME/area_51/github.com/LostRuins/koboldcpp.git/koboldcpp.py")
elif [[ -n "${KOBOLDCPP_BIN:-}" && -x "$KOBOLDCPP_BIN" ]]; then
  BIN=("$KOBOLDCPP_BIN")
elif command -v koboldcpp >/dev/null 2>&1; then
  BIN=($(command -v koboldcpp))
else
  echo "Error: can't find koboldcpp. Set KOBOLDCPP_PY (path to koboldcpp.py) or KOBOLDCPP_BIN (compiled binary)." >&2
  exit 3
fi

# ---- GPU selection + mutex ---------------------------------------------------
if [[ "$GPU_REGEX" == "cpu" ]]; then
  export CUDA_VISIBLE_DEVICES=
  TARGET_LAYERS="${LAYERS_OVERRIDE:-0}"
  IDX=""
else
  # robust index parse: CSV "index,name" → trim spaces/commas
  IDX=$(nvidia-smi --query-gpu=index,name --format=csv,noheader |
        awk -v pat="$GPU_REGEX" 'BEGIN{IGNORECASE=1; FS=","}
          $0 ~ pat {gsub(/^[ \t]+|[ \t]+$/,"",$1); print $1; exit}')
  [[ -z "$IDX" ]] && IDX=$(nvidia-smi --query-gpu=index --format=csv,noheader | head -n1 | tr -d '[:space:]')
  export CUDA_VISIBLE_DEVICES="$IDX"

  # If the chosen GPU is the 4090, stop comfyui first (mutex)
  if command -v herd >/dev/null 2>&1; then
    IDX4090=$(nvidia-smi --query-gpu=index,name --format=csv,noheader |
              awk 'BEGIN{IGNORECASE=1; FS=","} /4090/ {gsub(/^[ \t]+|[ \t]+$/,"",$1); print $1; exit}')
    if [[ -n "$IDX4090" && "$IDX" == "$IDX4090" ]]; then
      herd stop comfyui >/dev/null 2>&1 || true
    fi
  fi

  # ---- decide gpu_layers -----------------------------------------------------
  if [[ -n "$LAYERS_OVERRIDE" ]]; then
    TARGET_LAYERS="$LAYERS_OVERRIDE"
  else
    # exact block count via gguf-blocks if available; else filename heuristic
    if "$HOME/.local/bin/gguf-blocks" "$MODEL" >/dev/null 2>&1; then
      L_GUESS=$("$HOME/.local/bin/gguf-blocks" "$MODEL" || true)
    else
      base=$(basename "$MODEL"); L_GUESS=40
      shopt -s nocasematch
      [[ "$base" == *7b* || "$base" == *8b* ]] && L_GUESS=32
      [[ "$base" == *13b* ]] && L_GUESS=40
      [[ "$base" == *30b* ]] && L_GUESS=60
      [[ "$base" == *70b* ]] && L_GUESS=80
      shopt -u nocasematch
    fi

    free=$(nvidia-smi --id="$IDX" --query-gpu=memory.free --format=csv,noheader,nounits | head -n1 | tr -d '[:space:]')
    reserve=1024; avail=$(( free - reserve )); (( avail < 512 )) && avail=512
    fsz=$(stat -c%s "$MODEL" 2>/dev/null || stat -f%z "$MODEL" 2>/dev/null || echo 0)
    offload_bytes=$(( fsz * 75 / 100 ))
    (( L_GUESS <= 0 )) && L_GUESS=40
    per_layer_bytes=$(( offload_bytes / L_GUESS ))
    (( per_layer_bytes <= 0 )) && per_layer_bytes=$((64*1024*1024))
    avail_bytes=$(( avail * 1024 * 1024 ))
    TARGET_LAYERS=$(( avail_bytes / per_layer_bytes ))
    (( TARGET_LAYERS > L_GUESS )) && TARGET_LAYERS=$L_GUESS
    (( TARGET_LAYERS < 0 )) && TARGET_LAYERS=0
  fi
fi

echo "[kobold-launch] device=${CUDA_VISIBLE_DEVICES:-CPU} target_layers=$TARGET_LAYERS port=$PORT" >&2

# ---- launch with OOM backoff (no exec mid-loop) ------------------------------
TRY=$TARGET_LAYERS; ATT=0
while :; do
  ATT=$((ATT+1))
  echo "[kobold-launch] attempt $ATT --gpu-layers $TRY" >&2
  set +e
  "${BIN[@]}" \
    --host 0.0.0.0 --port "$PORT" \
    --threads 28 --blasbatchsize 256 --contextsize 8192 \
    --gpulayers "$TRY" \
    "$MODEL" 

  status=$?
  set -e
  if [[ $status -eq 0 ]]; then exit 0; fi
  if (( ATT >= 10 || TRY <= 0 )); then exit $status; fi
  TRY=$((TRY-2))
done
