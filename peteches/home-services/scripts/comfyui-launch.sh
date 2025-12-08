#!/usr/bin/env bash

set -euo pipefail
# Mutex: ensure Kobold isn't holding the 4090
herd stop kobold >/dev/null 2>&1 || true
idx=$(nvidia-smi --query-gpu=index,name --format=csv,noheader | awk 'BEGIN{IGNORECASE=1} /4090/ {print $1; exit}')
idx=${idx:-0}
export CUDA_VISIBLE_DEVICES="$idx"

cd /home/peteches/area_51/github.com/comfyanonymous/ComfyUI/
/home/peteches/area_51/github.com/comfyanonymous/ComfyUI/venv/bin/python /home/peteches/area_51/github.com/comfyanonymous/ComfyUI/main.py --disable-auto-launch --listen --enable-cors-header '*' --verbose
