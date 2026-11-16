#!/usr/bin/env bash
# media-normalise — offline re-encode of library video to the house format,
# then re-import through the owning Arr app (Radarr/Sonarr) so the new file
# lands in the right place and the replaced original is removed.
#
# House format (see peteches/services/media-normalise.scm for the rationale):
#   video   STREAM-COPIED, never re-encoded and never tone-mapped: HEVC/H.264 as
#           it is, HDR/PQ tags intact.  The WebOS client decodes Main 10 PQ
#           natively (measured end-to-end on the TV), so the old "flatten
#           everything to BT.709" rule bought nothing and cost days of GPU.
#           Exactly ONE video stream: remuxes with a bonus 1080p track confuse
#           this player, so extra video tracks are dropped by -map 0:v:0.
#   audio   every WebOS-decodable track copied (AC-3 / E-AC-3 / AAC / MP3), and
#           the incompatible ones (TrueHD, DTS, FLAC, PCM, Opus) dropped — they
#           are what used to force a transcode.  If a file has NO decodable
#           track at all, its loudest mix is encoded to AC-3 on CPU.
#   subs    nothing embedded.  Text subtitles are exported as sidecar
#           <stem>.<lang>.srt files beside the video; bitmap PGS/DVD tracks are
#           dropped (MP4 cannot carry them).  Measured on this client:
#           sidecars render fine, MP4-embedded mov_text is ignored entirely.
#   mux     MP4 with +faststart, so Jellyfin direct-plays it instead of running
#           ffmpeg at all — the fragmented-MP4 HLS path that remux forces is what
#           made the LG browser player tear down every ~23s.
#
# The script is idempotent: files already conforming are skipped, so re-runs
# are no-ops.  Nothing is ever deleted until the import has been verified.

set -euo pipefail

# jellyfin-ffmpeg is a portable build that dlopen()s the NVIDIA driver, and
# libcuda.so.1 lives in the system profile's lib dir; without this every
# -hwaccel cuda run dies with "Cannot load libcuda.so.1".  jellyfin-service-type
# does exactly this for its own transcoder (peteches/services/jellyfin.scm), and
# it belongs here rather than in the caller so manual runs get it too.
if [[ -e /run/current-system/profile/lib/libcuda.so.1 \
      && ":${LD_LIBRARY_PATH:-}:" != *":/run/current-system/profile/lib:"* ]]; then
  export LD_LIBRARY_PATH=/run/current-system/profile/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
fi

# verify() compares the staged file's duration against the file it was encoded
# from, so it must always be set — including when an encode is reused from
# staging (set -u would otherwise abort on an unbound variable there).
SRC_CURRENT=

# Run tallies, logged at the end: this job runs for hours, and "what did it
# actually do" should not require grepping the whole log.
N_SEEN=0 N_SKIP=0 N_REWRITTEN=0 N_SIDE=0 N_IMPORTED=0 N_FAILED=0

CONF=${MEDIA_NORMALISE_CONF:-/etc/media-normalise/config}
[[ -r $CONF ]] || { echo "media-normalise: cannot read $CONF" >&2; exit 1; }
# shellcheck source=/dev/null
source "$CONF"

: "${LIBRARY_ROOTS:?}" "${STAGING_ROOT:?}" "${QUALITY_NAME:?}"
: "${PROFILE_NAME:?}" "${LOG:?}"
# Only the rare fallback needs rate control — a source whose video codec cannot
# be stream-copied into MP4 at all (DivX/Xvid/MPEG-1/realvideo …).
: "${MAXRATE:=25000000}" "${CQ:=27}"
: "${STANDUP_APP:=radarr}"
# Which subtitle languages become sidecar files (space/comma separated, three-letter
# tags).  Empty means every text subtitle track — true to the intent but ruinous on
# this library's Blu-ray rips, which carry up to 38 of them.  Set it in the config.
: "${SUB_LANGS:=}"
# parsed   = hand back whatever quality the app read off the release name (honest:
#            a stream copy cannot change resolution).
# override = stamp QUALITY_NAME onto every import, as earlier versions did.
: "${QUALITY_MODE:=parsed}"

# log/die first: the QUALITY_MODE validation below calls die, and a function that is not
# yet defined costs an exit 127 instead of the message.
log() { printf '%s %s\n' "$(date -Is)" "$*" >>"$LOG"; printf '%s\n' "$*"; }
die() { log "ERROR: $*"; exit 1; }

case $QUALITY_MODE in parsed|override) : ;; *) die "QUALITY_MODE must be parsed or override" ;; esac
FF=${FFMPEG:-jellyfin-ffmpeg}
FP=${FFPROBE:-ffprobe}

DRY_RUN=0
[[ ${1:-} == "--dry-run" ]] && DRY_RUN=1

# ── app plumbing ────────────────────────────────────────────────────────────
# Verified live against this fleet's apps (Radarr 6.3.0, Sonarr 4.0.20): both still serve
# only under /api/v3 — a bare host:port answers 404, and `curl -f` throws away the body
# that says why, so every call fails with nothing to read.  The base is normalised here
# rather than trusted from config for that reason.
app_url() { local u; case $1 in radarr) u=${RADARR_URL:?};; sonarr) u=${SONARR_URL:?};; esac
            u=${u%/}; [[ $u == */api/v3 ]] || u="$u/api/v3"; echo "$u"; }
key_file() { case $1 in radarr) echo "${RADARR_API_KEY_FILE:?}";; sonarr) echo "${SONARR_API_KEY_FILE:?}";; esac; }

# Responses land in a scratch file so the HTTP status (via curl -w) and the body
# can be told apart.  That separation is the point: an Arr app always says *why*
# it refused inside the body, and `curl -f` discards the body — so every refusal
# used to arrive as "returned error: 400", with the actionable sentence thrown
# away.  The key never appears in any of this output.
API_BODY=$(mktemp "${TMPDIR:-/tmp}/media-normalise-api.XXXXXX") \
  || die "cannot create scratch file for API responses"
trap 'rm -f -- "$API_BODY"' EXIT

api() {  # app METHOD /path [json-body] -> body on stdout; non-2xx logs the app's reason
  local app=$1 m=$2 p=$3 d=${4:-} k code
  k=$(cat "$(key_file "$app")")
  local args=(-sS -X "$m" -H "X-Api-Key: $k" -w '%{http_code}' -o "$API_BODY")
  [[ -n $d ]] && args+=(-H 'Content-Type: application/json' --data "$d")
  code=$(curl "${args[@]}" "$(app_url "$app")$p" 2>/dev/null) \
    || { log "api: $app $m ${p%%\?*} got no response at all"; return 1; }
  [[ $code == 2* ]] \
    || { log "api: $app $m ${p%%\?*} -> HTTP $code: $(tr '\n' ' ' <"$API_BODY" | head -c 400)"
         return 1; }
  cat "$API_BODY"
}

# ── predicates ──────────────────────────────────────────────────────────────
# One ffprobe call measures everything the house definition asks about, as named
# key=value facts.  Parsing named fields out of JSON is deliberate: ffprobe
# reorders -show_entries CSV output, so column order is not the requested order.
# Caching it per file means conforming(), remux() and verify() all judge the same
# measurement of the same file — they cannot drift into disagreeing about what
# "conforming" means, which is what makes re-runs actually no-ops.
FACTS_FILE= FACTS=
#
# Stream indices are relative to their own type (audio index among audio streams,
# subtitle index among subtitle streams) because that is what ffmpeg's -map 0:a:N
# and -map 0:s:N mean; taking the position out of the flat .streams list happens
# to agree only when one video stream precedes them.
# What counts as decodable by the WebOS client, per Jellyfin's own device
# profile for it: AC-3, E-AC-3 and AAC pass; TrueHD/DTS/FLAC/PCM/Opus do not.
#
# SUB_LANGS is the subtitle allow-list (space, comma or semicolon separated,
# three-letter tags).  Empty means "every text subtitle", which a Blu-ray disc
# takes literally: this library has rips with 38 subtitle languages, and one
# sidecar each would bury the two anyone here actually reads.
probe_facts() {  # file -> one line of key=value facts
  $FP -v error \
      -show_entries stream=codec_type,codec_name,profile,pix_fmt,color_transfer,channels,duration,start_time \
      -show_entries stream_tags=language -show_entries format=format_name,duration -of json "$1" 2>/dev/null |
  jq -r --arg allow "${SUB_LANGS:-}" '
   # ffprobe says "N/A" when it has no measurement.  Facts spell that "-" so a reader
   # can never confuse "not measured" with a number — verify() depends on the
   # difference, since an unmeasured duration must fall back, not count as zero.
   def num(x): (x | tostring) as $s | if ($s | test("^-?[0-9]+([.][0-9]+)?$")) then $s else "-" end;
   (.streams // []) as $s
   | (($allow | ascii_downcase) | [ splits("[ ;,]+") ] | map(select(. != ""))) as $langs
   | ([ $s[] | select(.codec_type == "video") ][0] // {}) as $v
   | [ $s[] | select(.codec_type == "video") ] as $vid
   | [ $s[] | select(.codec_type == "audio") ] as $a
   | [ $s[] | select(.codec_type == "subtitle") ] as $sub
   # audio indexes are positions among AUDIO streams, which is what -map 0:a:N means
   | ([ $a | to_entries[]
       | select(.value.codec_name | IN("ac3", "eac3", "aac", "mp3")) | .key ] | join(",")) as $okidx
   | ([ $a | to_entries[]
       | select((.value.codec_name | IN("ac3", "eac3", "aac", "mp3")) | not) ] | length) as $badn
   | ([ $a[] | (.channels // 0) ] | max // 0) as $maxch
   | ([ $a | to_entries[] | select((.value.channels // 0) == $maxch) | .key ][0] // 0) as $best
   # subtitles: text codecs can be exported to SRT, bitmap ones cannot be kept at all.
   | [ $sub | to_entries[]
     | { i: .key,
         l: ((.value.tags.language // "und") | ascii_downcase),
         t: ((.value.codec_name // "") | IN("subrip", "ass", "ssa", "webvtt", "mov_text")) } ] as $subs
   # text tracks the allow-list rejects are counted separately from the ones the
   # codec rejects, because the log has to say which of the two dropped a subtitle.
   # The language has to be bound to a variable BEFORE piping into $langs: inside
   # index() the input is the array, so writing index(.l) makes jq evaluate .l
   # against that array and it dies with "Cannot index array with string", printing
   # nothing at all — which took the whole facts line down with it, since every fact
   # then read as empty and a 6.6 GiB HEVC Blu-ray was treated as an unknown-codec
   # source and re-encoded from scratch.
   | (if ($langs | length) == 0 then [ $subs[] | select(.t) ]
      else [ $subs[] | select(.t) | (.l as $lang | select(($langs | index($lang)) != null)) ] end) as $kept
   # With no allow-list nothing was dropped on language grounds, and counting them
   # anyway would make the log claim 37 subtitle tracks were skipped on a run that
   # skipped none.
   | (if ($langs | length) == 0 then 0
      else ([ $subs[] | select(.t) | (.l as $lang | select(($langs | index($lang)) == null)) ] | length) end) as $langdrop
   # One sidecar per LANGUAGE (lowest stream index wins): Jellyfin matches sidecars
   # by language, and two files for one language would fight over it.  The parens around
   # ($kept | group_by(.l)) matter — group_by returns ONE array of groups, so without
   # them `[]` iterates the wrapper instead of the groups, sort_by(.i) then indexes an
   # ARRAY with .i, jq dies printing nothing, and every fact downstream is empty.
   | ([ ($kept | group_by(.l))[] | sort_by(.i)[0] ] | sort_by(.i)) as $pick
   | [ "fmt=\(.format.format_name // "-")",
       # Timeline length plus the per-stream measurements verify() needs.  Matroska
       # stores no per-track duration at all (ffprobe answers N/A for video and audio),
       # so start_time is often the only real number here — kept_span_expect pairs it
       # with fmt_dur to get the span of a track.  Audio lists are audio-relative-indexed,
       # exactly like audio_ok_idx, so the two line up.
       "fmt_dur=\(num(.format.duration))",
       "v_dur=\(num($v.duration))", "v_start=\(num($v.start_time))",
       "audio_durs=\([$a[] | num(.duration)] | join(","))",
       "audio_starts=\([$a[] | num(.start_time)] | join(","))",
       "vcodec=\($v.codec_name // "-")",
       # spaces would break the token parser below, so profile stays underscored
       "vprofile=\(($v.profile // "-") | gsub(" "; "_"))",
       "vpix=\($v.pix_fmt // "-")", "vtrc=\($v.color_transfer // "unknown")",
       "video_n=\($vid | length)",
       "audio_n=\($a | length)",
       "audio_codecs=\([$a[] | .codec_name] | join(","))",
       "audio_channels=\([$a[] | (.channels // 0)] | join(","))",
       "audio_ok_idx=\($okidx)", "audio_bad_n=\($badn)",
       # the main surround mix: most channels wins, and its position varies by
       # disc (on some the first track is a stereo downmix or a commentary)
       "best_audio=\($best)", "best_channels=\($maxch)",
       "sub_n=\($sub | length)",
       "sub_text_n=\([$subs[] | select(.t)] | length)",
       "sub_bitmap_n=\([$subs[] | select(.t | not)] | length)",
       # text tracks kept vs dropped-by-language: with no allow-list these agree
       "sub_sidecars=\($pick | length)", "sub_lang_dropped=\($langdrop)",
       # "<subtitle-relative idx>:<language>" per sidecar to write, e.g.
       # "0:eng,2:spa" — comma-separated like audio_ok_idx, because a fact VALUE
       # may not contain whitespace: facts are read back by splitting on whitespace
       # (see fact()).  With spaces, only the FIRST spec survived the round trip and
       # every later language was dropped silently, since a shorter list is also a
       # smaller expectation in verify().
       "sidecar_specs=\([$pick[] | "\(.i):\(.l)"] | join(","))" ] | join(" ")'
}
facts() {  # file -> facts, probed once and reused while the file is current
  if [[ $FACTS_FILE != "$1" ]]; then FACTS=$(probe_facts "$1"); FACTS_FILE=$1; fi
}
fact() {  # key -> value from the cached facts (empty when absent; always succeeds)
  # Values must never contain whitespace, or they cannot be read back here — hence
  # comma-separated lists in probe_facts.
  local t
  for t in $FACTS; do [[ $t == "$1="* ]] && { printf '%s' "${t#*=}"; return 0; }; done
  return 0
}
facts_get() {  # facts-string key -> value.  verify() judges TWO files, and the cache
  # behind fact() always describes whichever was probed last (the staged one, by then),
  # so the source's own facts are held in a local string and read through this.
  local fs=$1 k=$2 t
  for t in $fs; do [[ $t == "$k="* ]] && { printf '%s' "${t#*=}"; return 0; }; done
  return 0
}

check_container() {  # MP4 whose moov precedes mdat, i.e. genuinely +faststart
  local f=$1 first
  facts "$f"
  case $(fact fmt) in *mp4*|*mov*) : ;; *) return 1 ;; esac
  # An early moov is what makes progressive HTTP seekable at all.  A coincidental
  # match of those four bytes inside mdat could read as faststart, so the test is
  # written to fail safe: no moov marker before the first mdat -> rewrite it once.
  first=$(head -c 1048576 "$f" | grep -aboE 'moov|mdat' | head -1)
  [[ ${first##*:} == moov ]]
}
check_video() {  # exactly one video track, in a codec MP4 can carry by copy
  facts "$1"
  [[ $(fact video_n) == 1 ]] || return 1
  case $(fact vcodec) in hevc|h264) return 0 ;; *) return 1 ;; esac
}
check_audio() {  # every track decodable, and at least one whenever audio exists
  facts "$1"
  local n bad ok
  n=$(fact audio_n); [[ $n == 0 ]] && return 0
  bad=$(fact audio_bad_n); [[ -z $bad || $bad == 0 ]] || return 1
  ok=$(fact audio_ok_idx); [[ -n $ok ]]
}
check_subs() {  # nothing embedded: subtitles live as sidecar .srt files instead
  facts "$1"
  [[ $(fact sub_n) == 0 ]]
}
# The single place that knows the whole house definition.  conforming() uses it
# silently; verify() logs every reason it reports.
conformity_failures() {  # file -> one reason per line; empty output means conforming
  local f=$1 out=()
  check_container "$f" || out+=("not a faststart MP4 (format=$(fact fmt))")
  check_video "$f"   || out+=("video not directly playable (tracks=$(fact video_n) codec=$(fact vcodec) profile=$(fact vprofile))")
  check_audio "$f"   || out+=("audio not wholly WebOS-decodable (codecs=$(fact audio_codecs) decodable_idx=$(fact audio_ok_idx) undecodable=$([[ -z $(fact audio_bad_n) ]] && echo 0 || fact audio_bad_n)/$(fact audio_n))")
  check_subs "$f"    || out+=("$(fact sub_n) embedded subtitle stream(s): text ones belong in sidecar .srt files, bitmap ones are dropped (text=$(fact sub_text_n) bitmap=$(fact sub_bitmap_n))")
  if (( ${#out[@]} )); then printf '%s\n' "${out[@]}"; fi
  return 0
}

conforming() {  # already house format?
  [[ -z $(conformity_failures "$1") ]]
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

# ── rewrite (stream copy) + verify ──────────────────────────────────────────
# Rewriting is a stream COPY, so cost is disk I/O only; the encoder is touched
# solely when a source's video codec cannot be carried by MP4 at all.
video_copyable() {  # current facts: can this codec go into MP4 untouched?
  case $(fact vcodec) in hevc|h264) return 0 ;; *) return 1 ;; esac
}

# The one place that writes an output file.  Subtitles are never mapped (they live
# as sidecars), data streams are dropped, and moov goes up front so the client can
# seek without Jellyfin ever needing to run ffmpeg for it.
run_mux() {  # src dst <maps and codec args>
  local src=$1 dst=$2; shift 2
  "$FF" -hide_banner -nostdin -loglevel error -y -i "$src" "$@" \
      -sn -dn -map_chapters -1 -map_metadata -1 \
      -movflags +faststart -f mp4 "$dst"
}

# What the house format keeps, worked out in ONE place so remux() writes exactly what
# verify() measures: until now both spelled out -map 0:v:0 and the decodable-audio rule
# separately, which is how a change to one could silently disagree with the other and
# make verify judge the rewrite against streams nobody asked for.
KEPT_MAPS=() KEEP_AOPTS=() KEEP_AUDIO_ENCODE=0 DROP_VIDEO=0 DROP_AUDIO=0 DROP_SUBS=0
kept_streams() {  # current facts -> KEPT_MAPS, KEEP_AOPTS, KEEP_AUDIO_ENCODE, DROP_*
  local -a idxs; local i n bad subs
  KEPT_MAPS=(-map 0:v:0)                 # exactly one video track: see the header note
  KEEP_AOPTS=(-c:a copy); KEEP_AUDIO_ENCODE=0
  if [[ -n $(fact audio_ok_idx) ]]; then
    # Copy every decodable track in stream order and drop the rest, so language
    # options survive and nothing unplayable is left behind.
    IFS=, read -r -a idxs <<<"$(fact audio_ok_idx)"
    for i in "${idxs[@]}"; do KEPT_MAPS+=(-map "0:a:$i"); done
  elif [[ $(fact audio_n) != 0 ]]; then
    # Nothing the client can decode at all (a TrueHD/DTS-only disc): one AC-3 mix.
    KEPT_MAPS+=(-map "0:a:$(fact best_audio)")
    KEEP_AOPTS=(-c:a ac3 -b:a 448k -ac "$(fact best_channels)"); KEEP_AUDIO_ENCODE=1
  fi
  n=$(fact video_n); case $n in ''|*[!0-9]*) n=0 ;; esac
  if (( n > 1 )); then DROP_VIDEO=$(( n - 1 )); else DROP_VIDEO=0; fi
  bad=$(fact audio_bad_n); case $bad in ''|*[!0-9]*) bad=0 ;; esac; DROP_AUDIO=$bad
  subs=$(fact sub_n); case $subs in ''|*[!0-9]*) subs=0 ;; esac; DROP_SUBS=$subs
}

remux() {  # src dst
  local src=$1 dst=$2
  facts "$src"; kept_streams
  if (( KEEP_AUDIO_ENCODE )); then
    log "audio: no WebOS-decodable track among [$(fact audio_codecs)] -> encoding an AC-3 $(fact best_channels)-channel mix from stream $(fact best_audio)"
  fi
  if video_copyable; then
    run_mux "$src" "$dst" "${KEPT_MAPS[@]}" -c:v copy "${KEEP_AOPTS[@]}"
  else
    # Rare: DivX/Xvid/MPEG-1/… that MP4 will not carry.  Re-encode, and let ffmpeg
    # pass the source colour metadata through with no -color_* flags of ours:
    # hevc_nvenc silently ignores primaries/trc, so stamping them used to produce
    # files whose tags contradicted each other.
    log "video: $(fact vcodec) cannot be stream-copied into MP4 -> re-encoding to HEVC Main 10, colour metadata passed through"
    run_mux "$src" "$dst" "${KEPT_MAPS[@]}" -c:v hevc_nvenc -preset p6 -profile:v main10 \
            -rc vbr -cq "$CQ" -maxrate "$MAXRATE" -bufsize $((MAXRATE * 2)) \
            -g 96 -keyint_min 96 "${KEEP_AOPTS[@]}"
  fi
}

# Text subtitles become sidecar .srt files beside the video.  Both halves of that
# were measured on the target client: an external .en.srt renders and leaves the
# stream direct-played, while MP4-embedded mov_text is never displayed at all.
# Bitmap PGS/DVD tracks are dropped outright — MP4 cannot carry them, and they are
# what used to force burn-in transcodes.  Count lands in SIDE_WRITTEN rather than
# stdout because log() writes there too.
# Split the sidecar list into the global SPECS array.  Both consumers go through
# this: the exporter below, and process_file's expected-count for verify().
SPECS=()
sidecar_specs() {  # current facts -> SPECS = "<idx>:<lang>" per sidecar to write
  local v; v=$(fact sidecar_specs); SPECS=()
  [[ -z $v ]] || IFS=, read -r -a SPECS <<<"$v"
}

# All sidecars for a file come out of ONE ffmpeg pass.  Doing it per subtitle
# stream (as the first version did) re-demuxes the whole disc every time: on a
# 7 GB MKV that measured ~90 s per language, so a 38-subtitle rip spent an hour on
# subtitles alone and the run looked hung.  One pass with one -map/-f srt pair per
# output reads the file once.
SIDE_WRITTEN=0
extract_sidecars() {  # src out-dir base
  local src=$1 dir=$2 base=$3 spec idx lang args missing
  SIDE_WRITTEN=0
  facts "$src"
  sidecar_specs
  (( ${#SPECS[@]} )) || return 0
  args=(-hide_banner -nostdin -loglevel error -y -i "$src")
  langs=(); idxs=()
  for spec in "${SPECS[@]}"; do
    idx=${spec%%:*}; lang=${spec##*:}
    args+=(-map "0:s:$idx" -f srt "$dir/$base.$lang.srt")
    langs+=("$lang"); idxs+=("$idx")
  done
  "$FF" "${args[@]}" >/dev/null 2>&1 \
    || log "WARNING subtitle: the sidecar pass for $base failed (see the per-language notes)"
  # Judge every output on its own: one track that will not convert must not cost
  # the others, and an empty file counts as a failure because Jellyfin would offer
  # a subtitle track with nothing in it.
  local i
  for i in "${!langs[@]}"; do
    if [[ -s "$dir/$base.${langs[$i]}.srt" ]]; then
      SIDE_WRITTEN=$(( SIDE_WRITTEN + 1 ))
    else
      rm -f -- "$dir/$base.${langs[$i]}.srt"
      log "WARNING subtitle: stream ${idxs[$i]} (${langs[$i]}) would not export to SRT; dropping it"
    fi
  done
  missing=$(( ${#SPECS[@]} - SIDE_WRITTEN ))
  log "subtitles: wrote $SIDE_WRITTEN sidecar(s) for $base in one pass ($missing not written)"
}

# ── how long the rewrite is ALLOWED to be ───────────────────────────────
# WHY THIS IS NOT JUST "the source's container duration".  A multi-version title — two
# video tracks, a DTS mix next to an AC-3, PGS subs — has ONE timeline whose end is set
# by whichever stream runs longest, INCLUDING the streams the house format throws away on
# purpose.  The rewrite keeps one video track plus the WebOS-decodable audio, so its own
# container duration is legitimately shorter: judging it against the source's container
# duration punishes a title for exactly the conformity the Arr app demanded, and it can
# never pass, so the rewrite sits in staging forever.  Measured live on
# /media/library/films/Backrooms (2026): every stream there starts at 4200s of a
# 10836.768s timeline and Matroska records no per-track duration, so the source tag reads
# 10836.768s while the honest span of what we keep is 6636.768s — which is exactly what
# the rewrite measured, to the millisecond.
#
# So the expectation comes from the KEPT streams: their own reported duration when ffprobe
# has one, else (Matroska writes none) timeline end minus that stream's start_time.  With
# one video track starting at 0 — every ordinary file — both reduce to today's comparison,
# so normal behaviour is unchanged.  When even that cannot explain the staged length, a
# dropped stream may be the reason, so MEASURE what the kept streams span instead of
# guessing; that extra read is paid for only by titles that would otherwise false-fail.
is_num() { [[ ${1:-} =~ ^-?[0-9]+([.][0-9]+)?$ ]]; }   # "-" / "N/A" / "" are not numbers
dur_close() { awk -v a="$1" -v b="$2" 'BEGIN{d=a-b; if(d<0)d=-d; exit (d>2)?1:0}'; }
drops_of() {  # source-facts -> "<video> <audio> <subs>" streams the house format throws away
  awk -v v="$(facts_get "$1" video_n)" -v a="$(facts_get "$1" audio_bad_n)" \
      -v s="$(facts_get "$1" sub_n)" '
    function n(x) { return (x ~ /^-?[0-9]+$/) ? x+0 : 0 }
    BEGIN { v=n(v); printf "%d %d %d", (v>1 ? v-1 : 0), n(a), n(s) }'
}

kept_span_expect() {  # source-facts -> "<seconds> <how it was measured>"; 1 if unmeasurable
  local fs=$1 max='' how='' fdur d st i cand na
  local -a ad as ok
  fdur=$(facts_get "$fs" fmt_dur)
  # Video: the house format keeps exactly v:0, so that is the only track to measure here.
  d=$(facts_get "$fs" v_dur); st=$(facts_get "$fs" v_start)
  if is_num "$d"; then max=$d; how="its reported stream duration"
  elif is_num "$fdur" && is_num "$st"; then
    max=$(awk -v a="$fdur" -v b="$st" 'BEGIN{printf "%.6f", a-b}')
    how="the source timeline end minus that stream's start_time"
  fi
  IFS=, read -r -a ad <<<"$(facts_get "$fs" audio_durs)"
  IFS=, read -r -a as <<<"$(facts_get "$fs" audio_starts)"
  ok=$(facts_get "$fs" audio_ok_idx)
  if [[ -z $ok ]]; then
    # Nothing decodable is still a kept stream: the rewrite encodes its one AC-3 mix from
    # best_audio, so that track's span bounds the output just as much as any copied one.
    na=$(facts_get "$fs" audio_n)
    case $na in ''|0|*[!0-9]*) ok= ;; *) ok=$(facts_get "$fs" best_audio) ;; esac
  fi
  IFS=, read -r -a ok <<<"$ok"
  for i in "${ok[@]}"; do
    case $i in ''|*[!0-9]*) continue ;; esac
    st="${as[$i]:-}"
    if is_num "${ad[$i]:-}"; then cand=${ad[$i]}
    elif is_num "$fdur" && is_num "$st";
    then cand=$(awk -v a="$fdur" -v b="$st" 'BEGIN{printf "%.6f", a-b}')
    else continue; fi
    if ! is_num "$max" || awk -v a="$cand" -v b="$max" 'BEGIN{exit !(a>b)}'; then
      max=$cand; how="the longest kept stream's span"
    fi
  done
  is_num "$max" && awk -v x="$max" 'BEGIN{exit !(x>0)}' \
    || { printf '%s %s' "$max" "$how"; return 1; }
  printf '%s %s' "$max" "$how"; return 0
}

# Measure the kept streams by muxing them on their own: this is the only ruler that can
# tell "we dropped a longer track" from "the mux stopped early", because both shorten the
# file.  Not circular — it re-derives the answer from the SOURCE, so a rewrite truncated
# mid-file still fails.  Costs one more read of the source plus a copy of what is kept,
# which is why verify() only reaches for it when a drop could be the explanation.
# The result lands in MEASURED_SPAN rather than stdout: remux() logs through log(), and
# log() writes to stdout too, so a command substitution would have swallowed those lines
# into the measurement (awk then reads them as 0 and every file "drifts").
MEASURED_SPAN=
kept_span_measured() {  # src dir -> sets MEASURED_SPAN; returns 1 when unmeasurable
  local src=$1 dir=$2 probe d rc=0
  MEASURED_SPAN=''
  probe=$(mktemp "$dir/.kept-span-XXXXXX") || return 1
  remux "$src" "$probe" || rc=$?
  if (( rc == 0 )); then
    d=$($FP -v error -show_entries format=duration -of csv=p=0 "$probe" 2>/dev/null || echo '')
    rm -f -- "$probe"
    is_num "$d" && { MEASURED_SPAN=$d; return 0; }
  else
    log "verify: could not re-mux the kept streams to measure them (source left as the only evidence)"
    rm -f -- "$probe"
  fi
  return 1
}

verify() {  # staged file [expected sidecar count] -> a valid house-format rewrite?
  local f=$1 want=${2:-0} fails l dur side stem esc src sfacts sdur exp expv how got dv da ds
  fails=$(conformity_failures "$f")
  if [[ -n $fails ]]; then
    while IFS= read -r l; do log "verify: $l"; done <<<"$fails"
    return 1
  fi
  # Subtitles are only "kept" if their sidecars actually landed next to the video:
  # a rewrite that silently lost them would still look conforming by stream facts.
  stem=$(basename "${f%.*}"); esc=$(printf '%s' "$stem" | sed -e 's/[][?*\\]/\\&/g')
  side=$(find "$(dirname "$f")" -maxdepth 1 -type f -name "$esc.*.srt" 2>/dev/null | wc -l)
  [[ $side -eq $want ]] || { log "verify: expected $want subtitle sidecar(s) beside $(basename "$f"), found $side"; return 1; }
  dur=$($FP -v error -show_entries format=duration -of csv=p=0 "$f" 2>/dev/null || echo 0)
  src=${SRC_CURRENT:-$f}
  # Fresh probe of the SOURCE: conformity_failures() above already swapped the facts
  # cache to the staged file, so fact() would silently answer about the wrong file here.
  sfacts=$(probe_facts "$src") || sfacts=''
  sdur=$(facts_get "$sfacts" fmt_dur); is_num "$sdur" || sdur=0
  exp=$(kept_span_expect "$sfacts") || exp=''
  if [[ -z $exp ]]; then
    # Nothing measurable about the source's streams at all: fall back to comparing whole
    # file durations — today's rule — rather than passing a file nobody measured.
    dur_close "$dur" "$sdur" \
      || { log "verify: duration drift ${dur}s vs source container ${sdur}s (no per-stream duration measurable)"
           return 1; }
    return 0
  fi
  expv=${exp%% *}; how=${exp#* }
  dur_close "$dur" "$expv" && return 0
  dv=0; da=0; ds=0; got=''; read -r dv da ds <<<"$(drops_of "$sfacts")"
  MEASURED_SPAN=''
  if (( dv + da + ds > 0 )); then
    log "verify: staged ${dur}s vs expected ${expv}s (${how}); the source drops video=$dv audio=$da subs=$ds, so measuring what the kept streams actually span"
    if kept_span_measured "$src" "$(dirname "$f")"; then
      got=$MEASURED_SPAN
      dur_close "$dur" "$got" \
        && { log "verify: ok — the kept streams span ${got}s and the staged file matches them (source container reads ${sdur}s because of those dropped streams)"
             return 0; }
    fi
  fi
  log "verify: duration drift ${dur}s vs expected ${expv}s (${how}); source container ${sdur}s, dropped video=$dv audio=$da subs=$ds"
  [[ -n $got ]] && log "  kept streams measured at ${got}s — the rewrite is shorter/longer than what we asked to keep"
  return 1
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

# The app's own view of one item's files, how to find the item that owns a folder, and
# how to tell the app to look at that folder again.
file_rows_for() {  # app item-id -> JSON array of {id,path} rows
  case $1 in
    radarr) api radarr GET "/moviefile?movieId=$2" | jq -c '[.[] | {id, path}]' ;;
    sonarr) api sonarr GET "/episodefile?seriesId=$2" | jq -c '[.[] | {id, path}]' ;;
  esac
}
file_ids_for() {  # app item-id -> space separated ids currently registered
  file_rows_for "$1" "$2" | jq -r '.[].id' | tr '\n' ' '
}

item_id_for_path() {  # app library-folder -> id of the movie/series that owns it
  local app=$1 d=$2
  case $app in
    radarr) api radarr GET /movie \
              | jq -r --arg d "$d" '[.[] | select(.path == $d)] | last // empty | .id' ;;
    # An episode lives in <series path>/Season NN, so the longest series path that is a
    # prefix of this folder is its owner.  Matching only equality would find nothing for TV.
    sonarr) api sonarr GET /series \
              | jq -r --arg d "$d" '[.[] | select($d == .path or ($d | startswith(.path + "/")))]
                                    | sort_by(.path | length) | last // empty | .id' ;;
  esac
}

# Telling the app to rescan its folder is what imports a file.  Both apps answer HTTP 201
# with a command record and do the scan a few seconds later in their own queue, so this
# only reports whether the request was accepted — callers poll for the result.
rescan_command() {  # app item-id -> 0 if the rescan was accepted
  case $1 in
    radarr) api radarr POST /command \
              "$(jq -nc --argjson i "$2" '{name: "RescanMovie", movieId: $i}')" >/dev/null ;;
    sonarr) api sonarr POST /command \
              "$(jq -nc --argjson i "$2" '{name: "RescanSeries", seriesId: $i}')" >/dev/null ;;
  esac
}

size_of() { wc -c <"$1" | tr -dc 0-9; }   # no stat on this PATH, and the file is local

# IMPORT BY RESCAN, NOT BY MANUAL IMPORT.  Both halves established against the live apps.
#
# Manual Import (POST /manualimport) accepts the request and then does nothing:
#   * measured on Radarr 6.3.0.10514 — a body built from its own folder listing plus the
#     flat .movieId answers 200 with [{"status":null,"rejections":[]}]; its debug log
#     stops at "UpgradeSpecification ... accepting"; /queue stays empty; no import command
#     runs; the file list is unchanged and the staged file never moves.  Identical result
#     with moveFiles=true and false, so it is not a transfer problem either.
#   * putting a GUID in .id gives HTTP 400 — the ids it hands back are integers, so they
#     have to be echoed as-is, which the code does by echoing the whole listing.
# None of that is diagnosable from here and nothing about it is fixable from here either,
# so this path does not depend on it.
#
# Rescan works, and was measured end to end: an unregistered MP4 placed in the movie's own
# folder plus POST {"name":"RescanMovie","movieId":124} (HTTP 201) became moviefile #823
# within about 15 s.  Sonarr accepts RescanSeries and RefreshSeries, and rejects made-up
# names with HTTP 500, which is how the two real ones were picked.
#
# Two consequences that matter:
#   * A rescan ADDS files rather than swapping them, so the original stays registered next
#     to the rewrite until WE delete it.  Deleting is therefore our job, has to happen
#     after the new file is confirmed registered, and must be scoped to the one file this
#     rewrite replaced — a season folder holds other episodes that are nobody else's
#     business.  The DELETE calls carry deleteFiles=true because without it the endpoint
#     forgets the record and leaves the bytes on disk.
#   * The app parses OUR FILE NAME when it registers, which is the honest quality reading
#     for a stream copy; there is no candidate object to argue with any more.  That makes
#     QUALITY_MODE=override a post-hoc patch of the registered file rather than part of an
#     import request (see below).
IMPORT_TRIES=${IMPORT_TRIES:-36}      # 5s apart: three minutes for a rescan to land it

import_item() {  # app staged-file staging-dir original -> 0 once the rewrite is registered and the original is not
  local app=$1 staged=$2 dir=$3 orig=$4 mid dest rows before after newid try oldid rc=0
  local n_in still sfx stem vsfx langpart newpath vdir pid qid srcdir
  srcdir=$(dirname "$orig")

  # The only handle a rescan needs is the item id.  If no movie or series claims this
  # folder then the library and the apps disagree, and staging it is all we can do.
  mid=$(item_id_for_path "$app" "$srcdir") || true
  [[ -n $mid && $mid != null ]] \
    || { log "import: no $app item owns $srcdir; leaving $staged in staging"; return 1; }

  dest="$srcdir/$(basename "$staged")"
  if [[ -e $dest ]]; then
    # A previous run got this far and stopped before the rescan.  Same size means it is
    # the same rewrite, so carry on from here rather than fail on our own leftovers; a
    # different size is somebody else's file, which we are not going to overwrite.
    if [[ $(size_of "$dest") == $(size_of "$staged") ]]; then
      log "import: rewrite already in place at $dest, continuing to the rescan"
      rm -f -- "$staged"
    else
      log "import FAILED: $dest already exists with a different size; staging kept for inspection"
      return 1
    fi
  fi

  before=$(file_ids_for "$app" "$mid")
  if [[ $DRY_RUN -eq 1 ]]; then
    log "import: dry-run, would move to $dest and rescan $app item $mid"
    return 0
  fi

  mv -- "$staged" "$dest" \
    || { log "import FAILED: could not move the rewrite into $srcdir"; return 1; }
  log "import: moved the rewrite to $dest and asked $app to rescan item $mid"
  rescan_command "$app" "$mid" \
    || { log "import FAILED: $app would not accept the rescan; the rewrite is in place at $dest"; return 1; }

  # Wait for it to be registered.  Deleting anything before this happens is how one run
  # left a movie with no file at all.  Registered means either an id we had not seen, or —
  # on a resumed run — our own path appearing in the list.
  newid= ; rows='[]'
  for try in $(seq 1 "$IMPORT_TRIES"); do
    sleep 5
    rows=$(file_rows_for "$app" "$mid") || continue
    after=$(jq -r '.[].id' <<<"$rows" | tr '\n' ' ')
    newid=$(comm -13 <(tr ' ' '\n' <<<"$before" | sort) <(tr ' ' '\n' <<<"$after" | sort) | head -1)
    [[ -n $newid ]] && break
    newid=$(jq -r --arg p "$dest" '.[] | select(.path == $p) | .id' <<<"$rows" | head -1)
    [[ -n $newid ]] && break
  done
  if [[ -z $newid ]]; then
    log "import FAILED: $app did not register $dest within $(( IMPORT_TRIES * 5 ))s"
    log "  the rewrite sits in the library folder and the original is untouched: $orig"
    return 1
  fi
  log "import: registered as file #$newid"

  # Now drop the file this rewrite replaced, by id, through the app, so its database goes
  # with the bytes.  Only that file: see the note above about season folders.
  oldid=$(jq -r --arg p "$orig" '.[] | select(.path == $p) | .id' <<<"$rows" | head -1)
  if [[ -n $oldid ]]; then
    log "import: removing replaced file #$oldid $orig"
    case $app in
      radarr) api radarr DELETE "/moviefile/$oldid?deleteFiles=true&addDeletedHistory=true" >/dev/null ;;
      sonarr) api sonarr DELETE "/episodefile/$oldid?deleteFiles=true&addDeletedHistory=true" >/dev/null ;;
    esac || log "import: WARNING $app would not remove file #$oldid; removing it directly"
    [[ -e $orig ]] && rm -f -- "$orig"
    rescan_command "$app" "$mid" || true          # so the app forgets the removed record too
  else
    log "import: note: $orig is not registered, so there is no file to remove"
  fi

  # Confirm OUR rewrite is registered and that no trace of the original remains.  Deliberately
  # not "exactly one video in the folder": a Sonarr season folder legitimately holds every
  # episode, and movie folders can hold samples, so that test false-failed successful TV
  # imports — and did so *before* the sidecar copy below, which is how subtitles went missing.
  # The second rescan needs its own moment to take effect, hence the short poll.
  mine=? still=?
  for try in $(seq 1 6); do
    sleep 5
    rows=$(file_rows_for "$app" "$mid") || continue
    mine=$(jq -r --arg p "$dest" '[.[] | select(.path == $p)] | length' <<<"$rows")
    still=$(jq -r --arg p "$orig" '[.[] | select(.path == $p)] | length' <<<"$rows")
    [[ $mine != 0 && $still == 0 ]] && break
  done
  if [[ ! $mine =~ ^[0-9]+$ || $mine -lt 1 || $still != 0 ]]; then
    log "import FAILED: wanted $dest registered and no sign of $orig (got $mine registration(s), original listed $still time(s)):"
    jq -r '.[] | "  #" + (.id|tostring) + " " + .path' <<<"$rows" |
      while IFS= read -r l; do log "$l"; done
    return 1
  fi

  # QUALITY_MODE=override patches the quality of the registered file.  Under a rescan the
  # app has already read our file name, and that reading is what the copy actually is —
  # override exists for an Arr profile deliberately cut around one quality ceiling, and it
  # now says so out loud rather than silently claiming a better resolution than we have.
  if [[ $QUALITY_MODE == override ]]; then
    qid=$(quality_id "$app" "$QUALITY_NAME") || true
    if [[ -n $qid && $qid != null ]]; then
      case $app in
        radarr) api radarr PUT "/moviefile/$newid" \
                  "$(jq -nc --argjson q "$qid" --arg n "$QUALITY_NAME" '{quality: {quality: {id: $q, name: $n}, revision: 1}}')" >/dev/null ;;
        sonarr) api sonarr PUT "/episodefile/$newid" \
                  "$(jq -nc --argjson q "$qid" --arg n "$QUALITY_NAME" '{quality: {quality: {id: $q, name: $n}, revision: 1}}')" >/dev/null ;;
      esac || log "import: WARNING quality $QUALITY_NAME could not be set on file #$newid"
      log "import: stamped $QUALITY_NAME on file #$newid because QUALITY_MODE=override"
    else
      log "import: WARNING QUALITY_NAME '$QUALITY_NAME' is unknown to $app; left its own parse alone"
    fi
  fi

  # Subtitle sidecars belong beside wherever the video ended up, under the video's own
  # stem: Jellyfin matches a sidecar to the file it shares a name with.  The app may have
  # renamed our file on the way in, so take both the directory and the stem from the path
  # the app reports rather than from staging.
  stem=$(basename "${staged%.*}")
  newpath=$(jq -r --argjson i "$newid" '.[] | select(.id == $i) | .path' <<<"$rows" 2>/dev/null || true)
  [[ -n $newpath && $newpath != null ]] || newpath=$dest
  vdir=$(dirname "$newpath"); vsfx=$(basename "${newpath%.*}")
  for sfx in "$dir/$stem."*.srt; do
    [[ -f $sfx ]] || continue
    langpart=${sfx##*"$stem."}
    [[ -e "$vdir/$vsfx.$langpart" ]] \
      || { cp -p -- "$sfx" "$vdir/$vsfx.$langpart" && log "subtitle: placed sidecar $vsfx.$langpart beside the imported file"; }
  done

  # Pin the item to the Normalised profile so the app never "upgrades" it back.
  pid=$(profile_id "$app" "$PROFILE_NAME") || true
  if [[ -n $pid && $pid != null ]]; then
    case $app in
      radarr) api radarr PUT "/movie/$mid" \
                "$(jq -c --argjson pid "$pid" '.qualityProfileId = $pid'
                      <<<"$(api radarr GET "/movie/$mid")")" >/dev/null ;;
      sonarr) api sonarr PUT "/series/$mid" \
                "$(jq -c --argjson pid "$pid" '.qualityProfileId = $pid'
                      <<<"$(api sonarr GET "/series/$mid")")" >/dev/null ;;
    esac || log "import: WARNING profile $PROFILE_NAME could not be set on the item"
    log "import: profile set to $PROFILE_NAME"
  else
    log "import: WARNING profile $PROFILE_NAME not found in $app; leaving profile untouched"
  fi
  log "import: ok, $dest is registered as #$newid and $(basename "$orig") is gone"
}

# ── per-file driver ─────────────────────────────────────────────────────────
process_file() {
  local src=$1 app dir staged base tag
  app=$(route "$src"); [[ $app == none ]] && return 0
  N_SEEN=$(( N_SEEN + 1 ))
  SRC_CURRENT=$src           # set before any branch: verify() always has a reference
  if conforming "$src"; then log "skip (conforming): $src"; N_SKIP=$(( N_SKIP + 1 )); return 0; fi
  if [[ $DRY_RUN -eq 1 ]]; then
    # A dry run that only says "would rewrite" is not actionable: name the reason,
    # because these are the numbers the whole policy is judged by.
    log "rewrite needed: $src"
    while IFS= read -r l; do [[ -n $l ]] && log "  why: $l"; done <<<"$(conformity_failures "$src")"
    return 0
  fi
  base=$(basename "$src"); base=${base%.*}
  # Staging is keyed on the source path, not just the file stem.  Two different
  # titles can easily share a stem (Sample.mp4, trailer.mp4, or generic episode
  # names across shows), and with a shared staging directory one title's finished
  # encode gets reused — and then imported — as another's: the import call only
  # ever sees "<stem>.mp4" inside folderPath, so nothing else could tell them
  # apart.  The hash keeps each source its own folder while staying deterministic,
  # so a staged encode is still reusable across runs.
  tag=$(printf '%s' "$src" | md5sum | cut -c1-8)
  dir="$STAGING_ROOT/$(split_of "$src")/$base-$tag"
  staged="$dir/$base.mp4"
  mkdir -p "$dir"
  # Cache: reuse a previously-staged rewrite rather than remuxing from scratch.
  # A file left by an earlier run (a verify or import failure) is already a
  # complete copy, so we only re-verify it.  This makes verify fixes cheap.
  want_side=0
  if [[ ! -s $staged ]]; then
    log "rewrite: $src -> $staged"
    if ! remux "$src" "$staged"; then
      log "rewrite FAILED: $src"; N_FAILED=$(( N_FAILED + 1 )); rm -rf -- "$dir"; return 0
    fi
    N_REWRITTEN=$(( N_REWRITTEN + 1 ))
    # Sidecars are produced from the SOURCE, so do it while its facts are current.
    facts "$src"; sidecar_specs; want_side=${#SPECS[@]}
    log "subtitles: ${#SPECS[@]} text subtitle track(s) to export (${SPECS[*]:-none})"
    extract_sidecars "$src" "$dir" "$base"
    N_SIDE=$(( N_SIDE + SIDE_WRITTEN ))
  else
    log "reusing cached rewrite: $staged"
    # The cached copy carries no record of how many sidecars it was meant to have,
    # so re-derive that from the source before verify() swaps the facts cache, and
    # make sure they exist — extract_sidecars skips ones already written, so a run
    # interrupted between remux and import resumes instead of failing forever.
    facts "$src"; sidecar_specs; want_side=${#SPECS[@]}
    log "subtitles: ${#SPECS[@]} text subtitle track(s) expected beside the cached rewrite (${SPECS[*]:-none})"
    extract_sidecars "$src" "$dir" "$base"
    N_SIDE=$(( N_SIDE + SIDE_WRITTEN ))
  fi
  if ! verify "$staged" "$want_side"; then
    # A rewrite nobody verified is not a success: counting it as one made the summary say
    # "rewritten=1 failed=0" for a title stuck in staging, which hides every stuck title
    # in an unattended run.  N_REWRITTEN keeps meaning "rewrites produced"; this is what
    # makes the difference visible.
    N_FAILED=$(( N_FAILED + 1 ))
    log "verify FAILED: $staged (rewrite left in staging UNIMPORTED, source untouched; counted as failed)"
    return 0
  fi
  if import_item "$app" "$staged" "$dir" "$src"; then
    rm -rf -- "$dir"
    N_IMPORTED=$(( N_IMPORTED + 1 ))
    log "done: $src"
  else
    N_FAILED=$(( N_FAILED + 1 ))
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
  local root nroots=0
  # LIBRARY_ROOTS is newline-separated (roots contain spaces, e.g. film titles);
  # iterate over the lines and quote each root so find does not word-split it.
  # A missing root used to fail inside the process substitution, whose status
  # nothing checked: the run looked fine and did nothing at all.
  while IFS= read -r root; do
    [[ -z $root ]] && continue
    if [[ ! -d $root ]]; then log "WARNING library root missing, skipped: $root"; continue; fi
    nroots=$(( nroots + 1 ))
    # Hidden directories are pruned: that is where this library keeps the files it
    # deliberately does not want — Radarr parks rejects in <movie folder>/.unwanted,
    # Jellyfin keeps its database in .queries, Samba recycles into #recycle.  Normalising
    # one of those means decoding a rejected rip into a brand-new MP4 beside it: work
    # nobody asked for, and it makes junk look intentional to Jellyfin.
    while IFS= read -r -d '' f; do
      case $f in
        *.mp4|*.mkv|*.m2ts|*.ts|*.avi|*.wmv|*.mpg|*.mpeg) process_file "$f" ;;
      esac
    done < <(find "$root" -type f -not -path '*/.*' -not -path '*/#*' -print0 | sort -z)
  done <<<"$LIBRARY_ROOTS"
  log "summary: $nroots root(s) walked; seen=$N_SEEN conforming=$N_SKIP rewritten=$N_REWRITTEN sidecars=$N_SIDE imported=$N_IMPORTED failed=$N_FAILED"
  log "=== media-normalise run end ==="
}

main "$@"
