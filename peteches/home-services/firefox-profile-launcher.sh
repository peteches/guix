#!/bin/sh
# Launcher: never parses args; passes them verbatim to Firefox.
# Selects profile from FIREFOX_PROFILE env var, else prompts via wofi.
# Map file format: "<profile_name>\t</path/to/wrapped/firefox>"
# Default map: ../share/firefox-wrappers.txt (relative to this script)
# Optional overrides via env:
#   FIREFOX_WRAPPER_MAP=/path/to/map
#   FIREFOX_PROFILE=name
# Optional config file (sourced if present): ../share/firefox-launcher.cfg
#   can set: wofi_cmd='wofi --dmenu --prompt "Select Firefox Profile:"'

set -eu

selfdir="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
mapf="${FIREFOX_WRAPPER_MAP:-"$selfdir/../share/firefox-wrappers.txt"}"
cfg="${selfdir}/../share/firefox-launcher.cfg"

wofi_cmd='wofi --dmenu --prompt "Select Firefox Profile:"'
[ -f "$cfg" ] && . "$cfg" 2>/dev/null || true

[ -r "$mapf" ] || { echo "Missing map file: $mapf" >&2; exit 1; }

profile="${FIREFOX_PROFILE:-}"

# If no profile set via env, prompt the user.
if [ -z "$profile" ]; then
  # Present profile names (first column) to wofi.
  # shellcheck disable=SC2013
  profile="$(cut -f1 "$mapf" | eval "$wofi_cmd")"
  [ -n "$profile" ] || { echo "No profile selected." >&2; exit 1; }
fi

# Resolve the firefox wrapper binary for the chosen profile.
bin="$(awk -v n="$profile" -F '\t' '$1==n{print $2}' "$mapf")"
[ -n "$bin" ] || { echo "Unknown profile: $profile" >&2; exit 1; }

# Launch Firefox with the profile; pass all original args verbatim.
nohup "$bin" -P "$profile" "$@" > /dev/null 2>&1 &
exit 0
