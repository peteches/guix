#!/bin/sh
set -eu

PROFILES_INI="$HOME/.mozilla/firefox/profiles.ini"

if [ ! -f "$PROFILES_INI" ]; then
  notify-send 'Firefox profiles' 'profiles.ini not found'
fi

# Build Name:Path list from profiles.ini
choices=$(awk -F= '
  /^\[Profile[0-9]+\]/{n="";p=""}
  /^Name=/{n=$2}
  /^Path=/{p=$2}
  /^$/{if(n!=""&&p!=""){print n":"p}}
  END{if(n!=""&&p!=""){print n":"p}}
' "$PROFILES_INI")

[ -n "$choices" ] || { notify-send 'Firefox profiles' 'No profiles defined'; exit 1; }

name=$(printf '%s\n' "$choices" | cut -d: -f1 | wofi --dmenu --prompt 'Select Firefox Profile:')
[ -z "$name" ] && exit 0

exec firefox -P "$name" --no-remote "$@"
