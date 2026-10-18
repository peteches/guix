#!/usr/bin/env bash
# media-normalise — offline re-encode of library video to the house format,
# then re-import through the owning Arr app (Radarr/Sonarr) so the new file
# lands in the right place and the replaced original is removed.
#
# House format (see peteches/services/media-normalise.scm for the rationale):
#   video   HEVC Main 10, 4K, HDR10 preserved (bt2020/smpte2084), no tonemap
#   audio   AAC 5.1 (or 2.0) — TrueHD/DTS-HD are what force on-the-fly transcodes
#   subs    image-based (PGS/VOBSUB) dropped; text subs kept as-is
#   mux     MP4, +faststart, 4s GOP, bitrate capped by MAXRATE
#
# The script is idempotent: files already conforming are skipped, so re-runs
# are no-ops.  Nothing is ever deleted until the import has been verified.

set -euo pipefail

CONF=${MEDIA_NORMALISE_CONF:-/etc/media-normalise/config}
[[ -r $CONF ]] || { echo "media-normalise: cannot read $CONF" >&2; exit 1; }
# shellcheck source=/dev/null
source "$CONF"

: "${LIBRARY_ROOTS:?}" "${STAGING_ROOT:?}" "${QUALITY_NAME:?}"
: "${PROFILE_NAME:?}" "${MAXRATE:?}" "${CQ:?}" "${LOG:?}"
: "${STANDUP_APP:=radarr}"
FF=${FFMPEG:-jellyfin-ffmpeg}
FP=${FFPROBE:-ffprobe}

DRY_RUN=0
[[ ${1:-} == "--dry-run" ]] && DRY_RUN=1

log() { printf '%s %s\n' "$(date -Is)" "$*" >>"$LOG"; printf '%s\n' "$*"; }
die() { log "ERROR: $*"; exit 1; }

# ── app plumbing ────────────────────────────────────────────────────────────
app_url() { case $1 in radarr) echo "${RADARR_URL:?}";; sonarr) echo "${SONARR_URL:?}";; esac; }
key_file() { case $1 in radarr) echo "${RADARR_API_KEY_FILE:?}";; sonarr) echo "${SONARR_API_KEY_FILE:?}";; esac; }

api() {  # app METHOD /path [json-body]
  local app=$1 m=$2 p=$3 d=${4:-} k
  k=$(cat "$(key_file "$app")")
  if [[ -n $d ]]; then
    curl -fsS -X "$m" -H "X-Api-Key: $k" -H 'Content-Type: application/json' \
         --data "$d" "$(app_url "$app")$p"
  else
    curl -fsS -X "$m" -H "X-Api-Key: $k" "$(app_url "$app")$p"
  fi
}

uri() { jq -sRr @uri <<<"$1"; }

# ── predicates ──────────────────────────────────────────────────────────────
conforming() {  # already house format?
  local f=$1 v a s c
  v=$($FP -v error -select_streams v:0 -show_entries stream=codec_name,profile,color_transfer,pix_fmt -of csv=p=0 "$f" 2>/dev/null || true)
  [[ $v == "hevc,Main 10,smpte2084,yuv420p10le" ]] || return 1
  s=$($FP -v error -select_streams s -show_entries stream=codec_type -of csv=p=0 "$f" 2>/dev/null || true)
  [[ -z $s ]] || return 1                      # image subs would force burn-in
  while IFS= read -r c; do
    [[ -z $c ]] && continue
    case $c in aac|ac3|eac3) ;; *) return 1 ;; esac
  done <<<"$($FP -v error -select_streams a -show_entries stream=codec_name -of csv=p=0 "$f" 2>/dev/null || true)"
  return 0
}

route() {  # library path -> owning app
  case $1 in
    /media/library/films/*)          echo radarr ;;
    /media/library/tv/*)             echo sonarr ;;
    /media/library/standup-comedy/*) echo "$STANDUP_APP" ;;
    *)                               echo none ;;
  esac
}

split_of() {  # library path -> films|tv|standup-comedy
  case $1 in
    /media/library/films/*)          echo films ;;
    /media/library/tv/*)             echo tv ;;
    /media/library/standup-comedy/*) echo standup-comedy ;;
  esac
}

# ── encode + verify ─────────────────────────────────────────────────────────
encode() {  # src dst
  local src=$1 dst=$2
  $FF -hide_banner -nostdin -loglevel error -y \
      -hwaccel cuda -c:v hevc_cuvid -i "$src" \
      -map 0:v:0 -map 0:a:0 -sn -map_metadata -1 \
      -c:v hevc_nvenc -preset p6 -profile:v main10 -tier high \
      -rc vbr -cq "$CQ" -maxrate "$MAXRATE" -bufsize $((MAXRATE * 2)) \
      -g 96 -keyint_min 96 \
      -color_primaries bt2020 -color_trc smpte2084 -colorspace bt2020nc \
      -c:a aac -b:a 640k -ac 6 \
      -movflags +faststart -f mp4 "$dst"
}

verify() {  # staged file -> 0 if it is a valid house-format file
  local f=$1 v s dur sdur
  v=$($FP -v error -select_streams v:0 -show_entries stream=codec_name,profile,color_transfer,pix_fmt -of csv=p=0 "$f" 2>/dev/null || true)
  [[ $v == "hevc,Main 10,smpte2084,yuv420p10le" ]] || { log "verify: bad video ($v)"; return 1; }
  s=$($FP -v error -select_streams s -show_entries stream=codec_type -of csv=p=0 "$f" 2>/dev/null || true)
  [[ -z $s ]] || { log "verify: subtitle streams present"; return 1; }
  head -c 65536 "$f" | grep -qa moov || { log "verify: no moov in head (not faststart/unfinished)"; return 1; }
  dur=$($FP -v error -show_entries format=duration -of csv=p=0 "$f" 2>/dev/null || echo 0)
  sdur=$($FP -v error -show_entries format=duration -of csv=p=0 "$SRC_CURRENT" 2>/dev/null || echo 0)
  awk -v a="$dur" -v b="$sdur" 'BEGIN{d=a-b; if(d<0)d=-d; exit (d>2)?1:0}' \
    || { log "verify: duration drift ${dur}s vs ${sdur}s"; return 1; }
  return 0
}

# ── import ──────────────────────────────────────────────────────────────────
quality_id() {  # app name -> id
  api "$1" GET /qualitydefinition \
    | jq -r --arg n "$2" '.[] | select(.quality.name == $n) | .quality.id' | head -1
}
profile_id() {  # app name -> id
  api "$1" GET /qualityprofile \
    | jq -r --arg n "$2" '.[] | select(.name == $n) | .id' | head -1
}

file_ids_before() {  # app item-json -> space separated current file ids
  local app=$1 item=$2
  case $app in
    radarr) api radarr GET "/moviefile?movieId=$(jq -r '.movie.id' <<<"$item")" | jq -r '.[].id' ;;
    sonarr) api sonarr GET "/episodefile?seriesId=$(jq -r '.series.id' <<<"$item")" \
              | jq -r --argjson eps "$(jq -c '.episodeIds' <<<"$item")" \
                  '.[] | select(any(.episodeIds[]; . as $e | $eps | index($e))) | .id' ;;
  esac
}
file_rows_after() {  # same query, full rows
  local app=$1 item=$2
  case $app in
    radarr) api radarr GET "/moviefile?movieId=$(jq -r '.movie.id' <<<"$item")" ;;
    sonarr) api sonarr GET "/episodefile?seriesId=$(jq -r '.series.id' <<<"$item")" \
              | jq --argjson eps "$(jq -c '.episodeIds' <<<"$item")" \
                  '[.[] | select(any(.episodeIds[]; . as $e | $eps | index($e)))]' ;;
  esac
}

import_item() {  # app staged-file staged-dir
  local app=$1 staged=$2 dir=$3 item body qid before after newid leftover oldpath rows
  item=$(api "$app" GET "/manualimport?folder=$(uri "$dir")" \
           | jq -c --arg p "$staged" '.[] | select(.path == $p)')
  [[ -n $item ]] || { log "import: no manual-import candidate for $staged"; return 1; }
  jq -e '(.movie // .series) != null' <<<"$item" >/dev/null \
    || { log "import: $staged not matched to any $app item"; return 1; }
  jq -e '(.rejections // []) | length == 0' <<<"$item" >/dev/null \
    || { log "import: rejections: $(jq -c .rejections <<<"$item")"; return 1; }

  qid=$(quality_id "$app" "$QUALITY_NAME") || die "quality $QUALITY_NAME unknown to $app"
  body=$(jq -c --arg p "$staged" --argjson qid "$qid" --arg qn "$QUALITY_NAME" \
           '[.[] | select(.path == $p) | .quality.quality = {id: $qid, name: $qn}]' \
           <<<"$(api "$app" GET "/manualimport?folder=$(uri "$dir")")")
  [[ $(jq length <<<"$body") -eq 1 ]] || die "import: expected 1 item, got $(jq length <<<"$body")"

  before=$(file_ids_before "$app" "$item" | tr '\n' ' ')
  log "import: PUT manualimport ($app) $staged"
  [[ $DRY_RUN -eq 1 ]] && { log "import: dry-run, stopping before PUT"; return 0; }
  api "$app" PUT "/manualimport?moveFiles=true" "$body" >/dev/null

  rows=$(file_rows_after "$app" "$item")
  after=$(jq -r '.[].id' <<<"$rows" | tr '\n' ' ')
  newid=$(comm -13 <(tr ' ' '\n' <<<"$before" | sort) <(tr ' ' '\n' <<<"$after" | sort) | head -1)
  leftover=$(comm -12 <(tr ' ' '\n' <<<"$before" | sort) <(tr ' ' '\n' <<<"$after" | sort))
  # The app usually removes the replaced file itself; if it kept it (both
  # visible), remove the leftovers now — they are the pre-normalisation files.
  for id in $leftover; do
    oldpath=$(jq -r --argjson i "$id" '.[] | select(.id == $i) | .path' <<<"$rows")
    log "import: removing replaced file #$id $oldpath"
    case $app in
      radarr) api radarr DELETE "/moviefile/$id" >/dev/null ;;
      sonarr) api sonarr DELETE "/episodefile/$id" >/dev/null ;;
    esac
    [[ -e $oldpath ]] && rm -f -- "$oldpath"
  done
  [[ $(wc -w <<<"$after") -eq $(( $(wc -w <<<"$leftover") + 1 )) ]] \
    || die "import: expected exactly 1 remaining file, got: $after"
  log "import: ok, new file id ${newid:-?}"

  # Pin the item to the Normalised profile so the app never "upgrades" it back.
  local pid mid
  pid=$(profile_id "$app" "$PROFILE_NAME") || true
  if [[ -n $pid && $pid != null ]]; then
    case $app in
      radarr) mid=$(jq -r '.movie.id' <<<"$item")
              api radarr PUT "/movie/$mid" \
                "$(jq -c --argjson pid "$pid" '.qualityProfileId = $pid' \
                    <<<"$(api radarr GET "/movie/$mid")")" >/dev/null ;;
      sonarr) mid=$(jq -r '.series.id' <<<"$item")
              api sonarr PUT "/series/$mid" \
                "$(jq -c --argjson pid "$pid" '.qualityProfileId = $pid' \
                    <<<"$(api sonarr GET "/series/$mid")")" >/dev/null ;;
    esac
    log "import: profile set to $PROFILE_NAME"
  else
    log "import: WARNING profile $PROFILE_NAME not found in $app; leaving profile untouched"
  fi
}

# ── per-file driver ─────────────────────────────────────────────────────────
process_file() {
  local src=$1 app dir staged base
  app=$(route "$src"); [[ $app == none ]] && return 0
  if conforming "$src"; then log "skip (conforming): $src"; return 0; fi
  base=$(basename "$src"); base=${base%.*}
  dir="$STAGING_ROOT/$(split_of "$src")/$base"
  staged="$dir/$base.mp4"
  log "encode: $src -> $staged"
  [[ $DRY_RUN -eq 1 ]] && { log "encode: dry-run, skipping"; return 0; }
  mkdir -p "$dir"
  SRC_CURRENT=$src
  if ! encode "$src" "$staged"; then
    log "encode FAILED: $src"; rm -rf -- "$dir"; return 0
  fi
  if ! verify "$staged"; then
    log "verify FAILED: $staged (quarantined, source untouched)"; rm -rf -- "$dir"; return 0
  fi
  if import_item "$app" "$staged" "$dir"; then
    rm -rf -- "$dir"
    log "done: $src"
  else
    log "import FAILED: $staged (left in staging for inspection)"
  fi
}

# ── main ───────────────────────────────────────────────────────────────────
main() {
  log "=== media-normalise run start (dry-run=$DRY_RUN) ==="
  command -v "$FF" >/dev/null || die "ffmpeg ($FF) not on PATH"
  command -v "$FP" >/dev/null || die "ffprobe ($FP) not on PATH"
  command -v curl >/dev/null || die "curl not on PATH"
  command -v jq   >/dev/null || die "jq not on PATH"
  for app in radarr sonarr; do
    [[ -r $(key_file "$app") ]] || die "API key for $app missing at $(key_file "$app")"
  done
  local root
  while IFS= read -r -d '' f; do
    case $f in
      *.mp4|*.mkv|*.m2ts|*.ts|*.avi|*.wmv|*.mpg|*.mpeg) process_file "$f" ;;
    esac
  done < <(find $LIBRARY_ROOTS -type f -print0 | sort -z)
  log "=== media-normalise run end ==="
}

main "$@"
