#!/bin/sh
# Simple selector that reads a map file "<name>\t</path/to/wrapped/firefox>"
# Map default location: ../share/firefox-wrappers.txt relative to this script.
# Optional: set FIREFOX_WRAPPER_MAP to override. Optional config: ../share/firefox-launcher.cfg with wofi=...

selfdir="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
mapf="${FIREFOX_WRAPPER_MAP:-"$selfdir/../share/firefox-wrappers.txt"}"
cfg="${selfdir}/../share/firefox-launcher.cfg"

wofi_cmd="wofi --dmenu --prompt 'Select Firefox Profile:'"
[ -f "$cfg" ] && . "$cfg" 2>/dev/null

choice="$1"; shift || true
if [ -z "$choice" ]; then
  # shellcheck disable=SC2162
  names=$(cut -f1 "$mapf")
  choice=$(printf "%s\n" $names | $wofi_cmd)
fi

bin=$(awk -v n="$choice" -F'\t' '$1==n{print $2}' "$mapf")
if [ -z "$bin" ]; then
  echo "Unknown profile: $choice" >&2
  exit 1
fi

exec "$bin" -P "$choice" "$@"
