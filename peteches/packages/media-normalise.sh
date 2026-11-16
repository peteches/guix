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
#   disc    a Blu-ray .iso/.img is not a container: ffprobe on the raw file answers
#           "Invalid data found when processing input", and forcing -f mpegts gives
#           garbage ("max resync size reached"), because a disc is a playback chain —
#           BDMV/PLAYLIST/*.mpls naming which BDMV/STREAM/*.m2ts clips make the feature,
#           in what order.  Disc images are therefore read through ffmpeg's libbluray
#           protocol as bluray://<path>, with the LONGEST relevant playlist selected and
#           then pinned (see disc_select, which refuses to guess when two titles are
#           within 5 % of each other).  A rewritten disc is no longer a disc, so it does
#           not keep the BR-DISK token: see disc_quality_token for the replacement.
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
# Pin/leftover counters are deliberately NOT folded into failed=: an import can be fully
# complete — rewrite registered, original unregistered and removed — and still leave the item
# on the wrong Arr profile, or its old bytes on disk.  Those get their own fields so that
# failed=0 cannot hide them (that is the same dishonesty verify() was fixed for) while
# failed=N keeps meaning "this title did not make it into the library".
# unreadable=/ambiguous= count files the walk FOUND but this tool could not judge: a container
# nothing here accepts (or that ffprobe cannot read at all), and a disc whose two longest titles
# are too close to call.  They get their own fields rather than being folded into conforming= —
# "conforming" is a claim about the file, and neither of these is one.
N_SEEN=0 N_SKIP=0 N_REWRITTEN=0 N_SIDE=0 N_IMPORTED=0 N_FAILED=0
N_PIN_FAILED=0 N_ORIG_LEFT=0 N_UNREADABLE=0 N_AMBIGUOUS=0

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
# The deployed value is the service record's sub-langs field (peteches/services/
# media-normalise.scm), currently "eng spa"; empty below only means "unset here".
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
# ── optical disc images (.iso / .img) ───────────────────────────────────────
# A disc is not a container ffmpeg can open: ffprobe on a raw .iso answers "Invalid data found
# when processing input" and forcing -f mpegts dies with "max resync size reached", because the
# file holds a playback chain (BDMV/PLAYLIST/*.mpls playlists naming which BDMV/STREAM/*.m2ts
# clips, in what order) rather than one MPEG-TS.  jellyfin-ffmpeg carries libbluray — its
# protocol list names bluray — so the disc is reachable as `bluray://<path>`: ffmpeg's own
# protocol hands the path to bd_open(), whose internal UDF reader understands an ISO image
# directly.  That means no loop mount and no bsdtar/7z/isoinfo/xorriso/mtools, all of which are
# absent on this wrapper PATH anyway (checked live on jellyfin: every one of them MISS), so
# nothing new has to be added to the package for disc support.
#
# Title selection is not invented here either.  ffmpeg's bluray protocol asks libbluray for the
# disc's RELEVANT playlists (bd_get_titles with TITLES_RELEVANT and MIN_PLAYLIST_LENGTH = 180 s,
# so menu segments never compete) and, when no playlist option is given, picks the LONGEST one —
# which is exactly the house rule "the feature is the longest title".  It logs every candidate
# with its duration plus which one it chose; disc_select reads that verdict off ffmpeg's own
# output and pins it with `-playlist <mpls-id>` for every later probe and mux of the same disc,
# so facts(), remux() and verify() all judge ONE title instead of each re-picking.  There is no
# `title` option to pass: a ?title=N query in a bluray URL reaches libbluray as part of the FILENAME
# it tries to open (measured live: "disc.c:333: failed opening UDF image ?title=1"), so query-string
# selection would fail on every disc, quietly.
DISC_PATH= DISC_URL= DISC_PLAYLIST= DISC_TOKEN= SRC_OPTS=()

disc_reset() {  # one disc at a time: never let the previous item's pseudo-URL leak into this one
  DISC_PATH= DISC_URL= DISC_PLAYLIST= DISC_TOKEN=; SRC_OPTS=()
}
is_disc() {  # path -> 0 when the extension means an optical-disc image
  case ${1,,} in *.iso|*.img) return 0 ;; *) return 1 ;; esac
}
# Everything that reads media resolves its input through here.  For an ordinary file that is the
# path itself with no extra options, so every ffmpeg/ffprobe command line this script has always
# built — including the ones verify() measures with — stays byte-for-byte what it was.
media_input() {  # library path -> what ffprobe/ffmpeg must be given to see this item's content
  if [[ ${DISC_PATH:-} == "$1" ]]; then printf '%s' "${DISC_URL:-$1}"; else printf '%s' "$1"; fi
}

# Playlist durations come from libbluray's own log, because nothing else on this box can ask a
# disc (no mount, no isoinfo):
#   [bluray @ 0x…] 8 usable playlists: / playlist 00000.mpls (1:36:04) / selected 00000.mpls
# Return codes are the caller's contract: 0 = a title was chosen, 1 = ambiguous (do NOT guess),
# 2 = libbluray cannot open this image at all.
disc_select() {  # disc path -> DISC_URL / DISC_PLAYLIST / SRC_OPTS set, and says what it picked
  local iso=$1 err rc=0 map sel line id d hms rid rd rhms bylen nrel why cid csec chms
  disc_reset
  err=$(mktemp "${TMPDIR:-/tmp}/media-normalise-disc.XXXXXX") || return 2
  # No -playlist here on purpose: this is the pass that ASKS ffmpeg what it would pick.
  $FP -hide_banner -v info -probesize 30M -show_entries format=duration \
      -of default=noprint_wrappers=1:nokey=1 "bluray://$iso" >/dev/null 2>"$err" || rc=$?
  map=$(sed -n 's/.*playlist \([0-9]\{5\}\)\.mpls (\([0-9]\{1,4\}\):\([0-9]\{2\}\):\([0-9]\{2\}\)).*/\1 \2 \3 \4/p' "$err")
  sel=$(sed -n 's/.*selected \([0-9]\{5\}\)\.mpls.*/\1/p' "$err" | head -1)
  nrel=0; nrel=$(printf '%s\n' "$map" | grep -c '[0-9]') || nrel=0
  if [[ -z $map ]]; then
    why=$(grep -m1 -iE 'bluray|udf|bd_open|not detected|aacs|bdplus' "$err" | head -c 200)
    log "disc: libbluray cannot open $(basename "$iso"): ${why:-ffmpeg reported no playlists at all (exit $rc)}"
    rm -f -- "$err"; return 2
  fi
  # hh:mm:ss -> seconds, longest first.  The human time is kept alongside so the log quotes the
  # disc's own numbers rather than a re-formatted guess at them.
  bylen=$(printf '%s\n' "$map" | awk '{ printf "%s %d %02d:%02d:%02d\n", $1, $2*3600+$3*60+$4, $2,$3,$4 }' | sort -k2,2rn)
  line=$(head -1 <<<"$bylen"); read -r id d hms <<<"$line" || true
  line=$(sed -n '2p' <<<"$bylen"); read -r rid rd rhms <<<"$line" || true
  # The ambiguity guard: two titles this close are a feature and an extended/cut version, or a
  # film and a nearly-equal bonus reel, and nothing here can tell which.  Guessing would import
  # the wrong cut as "the" movie and then delete the disc that held the right one.
  if [[ -n ${rid:-} ]] && awk -v a="$rd" -v b="$d" 'BEGIN{ exit ((b-a) <= 0.05*b) ? 0 : 1 }'; then
    log "disc: $(basename "$iso"): ambiguous titles — refusing to guess which is the feature"
    while IFS=' ' read -r cid csec chms; do [[ -n $cid ]] && log "  candidate: playlist $cid.mpls ($chms = ${csec}s)"; done <<<"$bylen"
    log "  rule: apply skips this disc until one title is more than 5 % longer than the runner-up"
    rm -f -- "$err"; return 1
  fi
  # ffmpeg's own pick wins where it reported one (it is what an unpinned mux would use anyway);
  # the measured longest is the fallback when a build logs candidates but not its choice.
  DISC_PLAYLIST=${sel:-$id}
  DISC_PATH=$iso; DISC_URL="bluray://$iso"; SRC_OPTS=(-playlist "$DISC_PLAYLIST")
  rm -f -- "$err"
  log "disc: $(basename "$iso"): selected playlist $DISC_PLAYLIST.mpls (${hms} = ${d}s), the longest of $nrel relevant title(s) libbluray reports"
  return 0
}

# The stem a rewritten disc lands under, with any disc-origin token taken out.  BR-DISK is the Arr
# apps' own word for the whole disc — a full remux/ISO — and Normalised-2160p (the profile this tool
# pins items to) deliberately lists no Remux/BR-DISK ceiling at all, so carrying that token into the
# name of one MP4 with one video track would keep claiming something the file is not.  Stripping it
# here is also what lets disc_landed_mates find an already-landed rewrite by the same rule.
disc_stem_base() {  # disc path -> stem without disc-origin tokens
  local b; b=$(basename "$1"); b=${b%.*}
  printf '%s\n' "$b" | sed -e 's/[[:space:]]*[-_ .]*BR-DISK\b//Ig' -e 's/[[:space:]]*[-_ .]*HD-DVD\b//Ig' \
                       -e 's/[[:space:]][[:space:]]*/ /g' -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//'
}

# What a copied disc IS, in the apps' own vocabulary.  Neither Radarr nor Sonarr has custom
# qualities, so the only honest claim is the string their file-name parser produces for this exact
# picture: bluray origin + measured height reads back as Bluray-<height>p (Radarr's naming convention,
# same family as the Bluray-720p it reports for 'Aliens (1986) Bluray-720p.mp4').  The height comes
# from probe_facts on this disc's own SELECTED title — never off the old file name, which is how a
# BR-DISK rip could sit beside a name that lied about it.  An unmeasurable or sub-720 picture gets
# NO token: there is no Bluray-SD spelling in either app to fall back on, and inventing one is the
# exact lie quality_token_for was written to stop.
disc_quality_token() {  # current facts -> the token an Arr app reads off a copied disc, else empty
  local h; h=$(fact vheight)
  case $h in ''|0|*[!0-9]*) printf ''; return 0 ;; esac
  if   (( h >= 2160 )); then printf 'Bluray-2160p'
  elif (( h >= 1080 )); then printf 'Bluray-1080p'
  elif (( h >=  720 )); then printf 'Bluray-720p'
  else printf '';
  fi
}

# "Already converted", for a disc: a conforming video sits beside it under the name this tool would
# have produced.  The DISC can never look conforming itself — its container is MPEG-TS playlists by
# definition — so without this an ISO whose rewrite already landed would be copied again, 21 GB at a
# time, on every single run.  Both stem shapes are tried: today's bare "<stem>.mp4" (what earlier
# versions landed, BR-DISK token and all) and "<stripped stem> <token>.mp4".  Every candidate still
# has to pass conforming(), so an unrelated or half-written file beside the disc never counts as one.
disc_landed_mates() {  # disc path -> one sibling video path per line, if any claims this disc
  local d stem stripped pat esc
  d=$(dirname "$1"); stem=$(basename "$1"); stem=${stem%.*}
  stripped=$(disc_stem_base "$1")
  for pat in "$stem" "$stripped"; do
    [[ -n $pat ]] || continue
    esc=$(printf '%s' "$pat" | sed -e 's/[][?*\\]/\\&/g')
    find "$d" -maxdepth 1 -type f -iname "$esc*.mp4" 2>/dev/null
  done | sort -u
}

# SUB_LANGS is the subtitle allow-list (space, comma or semicolon separated,
# three-letter tags).  Empty means "every text subtitle", which a Blu-ray disc
# takes literally: this library has rips with 38 subtitle languages, and one
# sidecar each would bury the two anyone here actually reads.
probe_facts() {  # file (or resolved media input) -> one line of key=value facts
  # -show_entries is a WHITELIST: a field that is not listed simply never appears in the JSON, and
  # a fact read off an absent field comes back empty without ever failing.  height was missing from
  # the list until a converted disc image landed with no quality token at all, because
  # disc_quality_token had nothing to measure — on disk ffprobe had reported 1920x1080 quite happily.
  $FP -v error \
      -show_entries stream=codec_type,codec_name,profile,pix_fmt,color_transfer,height,channels,duration,start_time \
      -show_entries stream_tags=language -show_entries format=format_name,duration \
      -of json "${SRC_OPTS[@]}" "$1" 2>/dev/null |
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
       # height is what a copied disc image needs in order to claim a quality token
       # (disc_quality_token); nothing else here reads it, so this adds a fact without
       # touching any verdict line.
       "vheight=\($v.height // 0)",
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
  # Cached on the RESOLVED input, not the path: a disc's facts are facts about one playlist of
  # that disc, and they must not be handed to some other item that happens to share a stem.
  local in; in=$(media_input "$1")
  if [[ $FACTS_FILE != "$in" ]]; then FACTS=$(probe_facts "$in"); FACTS_FILE=$in; fi
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
  local src=$1 dst=$2 in; shift 2
  in=$(media_input "$src")
  "$FF" -hide_banner -nostdin -loglevel error -y "${SRC_OPTS[@]}" -i "$in" "$@" \
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
  args=(-hide_banner -nostdin -loglevel error -y "${SRC_OPTS[@]}" -i "$(media_input "$src")")
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
  # A disc's source is its bluray:// pseudo-URL with the pinned playlist — probing the raw .iso
  # would measure nothing at all ("Invalid data found"), and verify() would then be comparing the
  # rewrite against an unmeasured source instead of against the title it was copied from.
  sfacts=$(probe_facts "$(media_input "$src")") || sfacts=''
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

# ── naming: the quality token a rewrite is allowed to claim ─────────────────
# A rewrite is a stream copy, so it cannot change resolution or origin: the only quality
# token that is true of it is the one the app already records for the file being replaced.
# Both apps store that as the exact string their own file-name parser reads back — measured
# live on Radarr 6.3.0 and Sonarr 4.0.20, where movieFile / episodeFile .quality.quality.name
# is "Bluray-720p" for 'Aliens (1986) Bluray-720p.mp4' and "SDTV" for a '... SDTV.mp4'
# episode file.  Echoing it back means the app reads off the new name exactly what it had
# already decided, instead of Unknown — which is what today's bare <stem>.mp4 lands as.
# Revision is echoed with the apps' own words only (-REPACK / -PROPER from revision.isRepack /
# real == 2); a bare version number has no spelling in either app's naming convention, so we
# do not invent one (measured: 0 of 40 sampled Radarr movie files carry any revision at all).
# Empty output means NO token — unregistered file, an item that reports no quality, "Unknown"
# (the app admitting it did not work out), or a reply we could not parse.  In every one of
# those the rewrite lands under today's bare <stem>.mp4: naming it after a quality nobody
# measured is the exact lie this replaces.
# Both helpers below RETURN their value on stdout, so every message they log must go to the
# log FILE only: log()'s second printf echoes to stdout, and command substitution would fold
# that sentence into the returned name (measured live — the first apply staged a file named
# after the whole log line because of nothing else).
quality_token_for() {  # app library-file -> the quality token that app records for it, else empty
  local app=$1 f=$2 mid rows tok try
  case $app in radarr|sonarr) : ;; *) return 0 ;; esac
  mid=$(item_id_for_path "$app" "$(dirname "$f")") || true
  if [[ -z ${mid:-} || $mid == null ]]; then
    log "name: no $app item owns $(dirname "$f"), so $(basename "$f") lands untokened" >/dev/null
    return 0
  fi
  # The UNTRIMMED file list.  file_rows_for() drops .quality on purpose — the import loops
  # only need ids — and this is the one reader that wants the meaning.  Retried like every
  # other read here (see item_json): a garbage reply must not be mistaken for "no quality",
  # so it is retried until it actually parses as an array or the tries run out.
  tok=
  for try in $(seq 1 "$API_TRIES"); do
    rows=$( case $app in
              radarr) api radarr GET "/moviefile?movieId=$mid" ;;
              sonarr) api sonarr GET "/episodefile?seriesId=$mid" ;;
            esac ) || true
    if [[ -n ${rows:-} ]] && jq -e 'type == "array"' >/dev/null 2>&1 <<<"$rows"; then
      tok=$(jq -r --arg p "$f" '
              [ .[] | select(.path == $p) ] | last // empty
              | (.quality.quality.name // "") as $n
              | (if ((.quality.revision.isRepack // false)) then "-REPACK"
                 elif ((.quality.revision.real // 0) == 2) then "-PROPER" else "" end) as $rev
              | if ($n == "" or ($n | ascii_downcase) == "unknown") then "" else $n + $rev end
            ' <<<"$rows" 2>/dev/null) || tok=
      [[ -z $tok || $tok == null ]] && tok=
      break
    fi
    (( try < API_TRIES )) && sleep "$API_RETRY_SLEEP"
  done
  if [[ -z $tok ]]; then
    log "name: $app records no readable quality for $(basename "$f") (item $mid); landing it untokened rather than inventing one" >/dev/null
    return 0
  fi
  printf '%s' "$tok"
}

# The stem the rewrite lands under: the original's own stem, plus that token, separated by a
# space — which is how both apps name files themselves.  When the stem already carries the
# token (most library files do; that is where the app read it from) appending it again would
# be noise rather than honesty, so the stem goes through unchanged.
name_stem_for() {  # app original-file -> the stem the rewrite should land under
  local app=$1 f=$2 base tok
  if is_disc "$f"; then
    # A disc image gets neither the token the app recorded nor its own file name as-is: BR-DISK is
    # the word for the DISC, and this rewrite is one MP4 with one video track.  The stem drops that
    # token (disc_stem_base) and the claim comes from what was MEASURED on the selected title
    # (disc_quality_token) — the same evidence an Arr app would use to name the file itself.
    base=$(disc_stem_base "$f"); tok=${DISC_TOKEN:-}
  else
    base=$(basename "$f"); base=${base%.*}
    tok=$(quality_token_for "$app" "$f") || tok=
  fi
  if [[ -z $tok ]]; then printf '%s' "$base"; return 0; fi
  if [[ ${base,,} == *"${tok,,}"* ]]; then
    log "name: $app records $tok for $(basename "$f") and its name already says so — stem kept as is" >/dev/null
    printf '%s' "$base"
  else
    log "name: landing \"$base $tok\" because $tok is the quality $app already records for $(basename "$f")" >/dev/null
    printf '%s %s' "$base" "$tok"
  fi
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

# How long to wait for the apps to be coherent.  While a rescan or a delete is still running
# they answer with something that is not JSON at all (see item_json), so every read that gets
# believed is retried.  API_RETRY_SLEEP doubles as the knob a test harness sets to 0.
API_TRIES=${API_TRIES:-3} PIN_TRIES=${PIN_TRIES:-3} PIN_VERIFY_TRIES=${PIN_VERIFY_TRIES:-3}
API_RETRY_SLEEP=${API_RETRY_SLEEP:-5}

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
#     to the rewrite until WE deal with it — and "then delete the original" only works if the
#     app was ever going to register the rewrite.  It was not: measured live on Radarr 6.3.0
#     movie 140 ("Loop Track"), whose profile is Any (#1) and whose quality order puts
#     bluray-1080p-brdisk ahead of bluray-1080p-none, bluray-1080p-remux and the Bluray-2160p
#     tiers, a conforming 17,565,978,130 B rewrite sat in the folder UNREGISTERED for over 40
#     minutes while file #784 (the ISO) kept the movie's file slot.  A rescan imports a
#     candidate the app rates as an upgrade, and under that profile a stream copy of the same
#     picture is not one while the original holds the slot.  So the order is: ask for the
#     rewrite; if it will not register and the original still does, UNREGISTER the original —
#     DELETE with deleteFiles=false, which drops the record and keeps every byte — rescan
#     again, and only once OUR file is registered let anything touch those bytes.  Nothing is
#     ever deleted before our own file is in the library.  Deleting by id is still scoped to
#     the one file this rewrite replaced (a season folder holds other episodes that are nobody
#     else's business), deleteFiles=true is used only while the app still knows that id, and
#     after an unregister it does not, so those bytes come off with rm.
#   * The app parses OUR FILE NAME when it registers, which is the honest quality reading
#     for a stream copy; there is no candidate object to argue with any more.  That makes
#     QUALITY_MODE=override a post-hoc patch of the registered file rather than part of an
#     import request (see below).
IMPORT_TRIES=${IMPORT_TRIES:-36}      # 5s apart: three minutes for a rescan to land it

# GET one item, retried until the reply actually parses as JSON.  The apps do answer with
# something unparseable while they churn — measured live on Radarr 6.3.0: GET /movie/128 came
# back such that jq died with "Invalid numeric literal at line 1, column 31", and the same
# call moments later parsed fine.  One read is therefore not evidence in either direction.
item_json() {  # app collection id -> that item's JSON, retried until it parses as an object
  local app=$1 coll=$2 id=$3 body try
  for try in $(seq 1 "$API_TRIES"); do
    if body=$(api "$app" GET "/$coll/$id") && [[ -n $body ]] \
       && jq -e 'type == "object"' >/dev/null 2>&1 <<<"$body"; then
      printf '%s' "$body"
      return 0
    fi
    sleep "$API_RETRY_SLEEP"
  done
  return 1
}

# Pin an item to PROFILE_NAME, say out loud what happened, and count it when it did not take.
# Two faults this replaces: one unverified PUT whose "profile set" line printed even when the
# reply had been garbage (live evidence — Radarr movie 128 still reports profile 1 "Any" after
# a run that logged "import: profile set to Normalised-2160p"), and no tolerance at all for
# that transient garbage, which turned a momentary bad reply into a permanent-looking one.
# The PUT body is the item as the app currently holds it, with only qualityProfileId changed;
# success is decided by reading the item back, never by the PUT's own status code.  This runs
# after the rewrite is registered and the original removed, so failure here is reported and
# counted (N_PIN_FAILED) rather than fatal: an imported title still sitting on the wrong profile
# is not a success, but it is also not something we can undo by refusing to return 0.
PIN_READ=
pin_item_to_profile() {  # app movie/series-id -> 0 once the item reports PROFILE_NAME's id
  local app=$1 mid=$2 coll pid body got try seen
  PIN_READ=
  case $app in radarr) coll=movie ;; sonarr) coll=series ;; esac
  pid=$(profile_id "$app" "$PROFILE_NAME") || true
  if [[ -z $pid || $pid == null ]]; then
    N_PIN_FAILED=$(( N_PIN_FAILED + 1 ))
    log "import: WARNING profile $PROFILE_NAME is not known to $app, so nothing could be pinned (counted as pin-failed)"
    return 1
  fi
  for try in $(seq 1 "$PIN_TRIES"); do
    if body=$(item_json "$app" "$coll" "$mid") &&
       api "$app" PUT "/$coll/$mid" \
          "$(jq -c --argjson pid "$pid" '.qualityProfileId = $pid' <<<"$body")" >/dev/null; then
      for seen in $(seq 1 "$PIN_VERIFY_TRIES"); do
        sleep "$API_RETRY_SLEEP"
        got=$(item_json "$app" "$coll" "$mid" | jq -r '.qualityProfileId // empty' 2>/dev/null) || got=
        PIN_READ=$got
        [[ $got == "$pid" ]] \
          && { log "import: profile set to $PROFILE_NAME (#$pid) on item $mid, confirmed by reading it back from $app"; return 0; }
      done
    fi
    (( try < PIN_TRIES )) && sleep "$API_RETRY_SLEEP"
  done
  N_PIN_FAILED=$(( N_PIN_FAILED + 1 ))
  log "import: WARNING profile $PROFILE_NAME could not be pinned on item $mid of $app (it still reads ${PIN_READ:-no parsable reply}); counted as pin-failed, the import itself completed"
  return 1
}

# IMPORT_POLL_SLEEP spaces out one registration poll.  A full window is spent on each of the
# two phases import_item runs (ask for it, then ask again after unregistering the original),
# so an item that needs both takes twice as long and says so in its own log lines.
IMPORT_POLL_SLEEP=${IMPORT_POLL_SLEEP:-5}
LAST_ROWS='[]'          # the listing that settled the last poll, for whoever reports it

# One read of a file listing against the "is this ours?" rules below; no sleeping, so both the
# resume check and the polls can use it.  Prints the id, or fails without printing anything.
registration_id_in() {  # rows-json dest orig before-ids -> our file id if that listing has it registered
  local rows=$1 dest=$2 orig=$3 before=$4 got i p dstdir
  dstdir=$(dirname "$dest")
  got=$(jq -r --arg p "$dest" '[.[] | select(.path == $p)] | last // empty | .id' <<<"$rows" 2>/dev/null) || true
  [[ $got =~ ^[0-9]+$ ]] && { printf '%s' "$got"; return 0; }
  while IFS=$'\t' read -r i p; do
    case " $before " in *" $i "*) continue ;; esac     # already registered when we started
    [[ $p == "$orig" ]] && continue                    # the original re-registering is not us
    [[ $(dirname "$p") == "$dstdir" ]] || continue     # another item's folder is none of our business
    [[ ${p,,} == *.mp4 ]] || continue                  # our rewrite is an mp4; a re-registered disc is not
    printf '%s' "$i"; return 0
  done < <(jq -r '.[] | "\(.id)\t\(.path)"' <<<"$rows" 2>/dev/null)
  return 1
}

# Has $app registered OUR rewrite yet?  Returns 0 with OUTVAR set to our file id, and leaves
# LAST_ROWS holding the listing that proved it; otherwise it fails after the whole window.
# The result travels through a nameref instead of stdout on purpose: command substitution runs
# in a subshell, which would throw LAST_ROWS away — measured live, and it is exactly how an
# earlier version of this code took the "nobody is registered" branch on movie 140 while file
# #784 was sitting there registered.  "Ours" means either the exact path we placed (the normal
# case), or a file id that was not registered when we started AND is not the original's own
# path: the app renames files on import whenever its naming settings say so, so matching only
# our own path false-fails a successful import.  Excluding the original by PATH is what makes
# this safe after an unregister — those bytes are still in the folder, so the next rescan can
# hand back their old id (or a brand-new one for the same file) as a "newly registered" entry,
# and a plain ids-diff would report that as our rewrite landing.
rewrite_registration() {  # app item-id dest orig before-ids OUTVAR -> 0 once ours is registered
  local app=$1 mid=$2 dest=$3 orig=$4 before=$5 out=$6 rows t found
  local -n _ret=$out
  LAST_ROWS='[]'
  for t in $(seq 1 "$IMPORT_TRIES"); do
    sleep "$IMPORT_POLL_SLEEP"
    rows=$(file_rows_for "$app" "$mid") || continue
    LAST_ROWS=$rows
    found=$(registration_id_in "$rows" "$dest" "$orig" "$before") || true
    [[ $found =~ ^[0-9]+$ ]] && { _ret=$found; return 0; }
  done
  return 1
}

# The quality token this rewrite claims, read off the name it is landing under: name_stem_for
# built that stem out of the original's own stem plus exactly this suffix, so stripping the
# stem back off recovers the claim.  Only ever used to complete a log sentence.
rewrite_token_of() {  # orig-file dest -> the quality token in dest's name, else its whole name
  local base want tok
  want=$(basename "${2%.mp4}")
  for base in "$(disc_stem_base "$1")" "$(basename "$1")"; do
    base=${base%.*}
    [[ -n $base && $want == "$base"* ]] || continue
    tok=${want#"$base"}; tok=${tok# }; tok=${tok#.}
    printf '%s' "${tok:-no quality token}"; return 0
  done
  printf '%s' "$want"
}

import_item() {  # app staged-file staging-dir original -> 0 once the rewrite is registered and the original is not
  local app=$1 staged=$2 dir=$3 orig=$4 mid dest rows before newid try oldid rc=0 already=0
  local n_in still sfx stem vsfx langpart newpath vdir qid srcdir fcoll got need_move
  srcdir=$(dirname "$orig")

  # The only handle a rescan needs is the item id.  If no movie or series claims this
  # folder then the library and the apps disagree, and staging it is all we can do.
  mid=$(item_id_for_path "$app" "$srcdir") || true
  [[ -n $mid && $mid != null ]] \
    || { log "import: no $app item owns $srcdir; leaving $staged in staging"; return 1; }

  dest="$srcdir/$(basename "$staged")"
  # A landed-but-unregistered rewrite is handed to us already sitting at $dest (see the disc
  # branch in process_file), so "the staging copy" and "the file in the library folder" are the
  # same inode.  Deleting one deletes the rewrite itself — that rm below has to know when it is
  # looking at its own tail.
  if [[ $staged == "$dest" ]]; then
    log "import: $(basename "$dest") already sits in the library folder unregistered; nothing to move"
    need_move=0
  elif [[ -e $dest ]]; then
    # A previous run got this far and stopped before the rescan.  Same size means it is
    # the same rewrite, so carry on from here rather than fail on our own leftovers; a
    # different size is somebody else's file, which we are not going to overwrite.
    if [[ $(size_of "$dest") == $(size_of "$staged") ]]; then
      log "import: rewrite already in place at $dest, continuing to the rescan"
      rm -f -- "$staged"; need_move=0
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

  # The resume path above already moved it (and dropped the staging copy), so do not try to
  # move a file that is no longer there: measured against the old code, that meant every
  # resumed run died here with "could not move the rewrite" instead of finishing the import.
  if (( need_move )); then
    mv -- "$staged" "$dest" \
      || { log "import FAILED: could not move the rewrite into $srcdir"; return 1; }
    log "import: moved the rewrite to $dest"
  fi
  rescan_command "$app" "$mid" \
    || { log "import FAILED: $app would not accept the rescan; the rewrite is in place at $dest"; return 1; }
  # Logged after the request was accepted, not before it: the old line claimed we had asked
  # for the rescan one command before asking, so a refusal still read as if we had asked.
  log "import: asked $app to rescan item $mid"

  # ── phase 1: OUR rewrite has to be registered before anything may touch a byte. ──
  # Deleting first is what used to leave a movie with no file at all; asking and assuming is
  # what left movie 140 with an unregistered 21 GB rewrite beside a registered ISO.
  newid= ; already=0
  if [[ $need_move -eq 0 ]] && rows=$(file_rows_for "$app" "$mid") \
     && newid=$(registration_id_in "$rows" "$dest" "$orig" ""); then
    # Resume case (a): an earlier run got the rewrite registered and something after that
    # failed.  The title is already in the library under exactly the name we would have given
    # it, so no rescan is asked for; phase 2 still gets its chance at any original left behind.
    already=1
    log "import: $dest is already registered with $app as file #$newid (a previous run imported it)"
  else
    if rewrite_registration "$app" "$mid" "$dest" "$orig" "$before" newid; then
      rows=$LAST_ROWS
      log "import: registered as file #$newid"
    else
      rows=$LAST_ROWS
      oldid=$(jq -r --arg p "$orig" '[.[] | select(.path == $p)] | last // empty | .id' <<<"$rows")
      if [[ $oldid =~ ^[0-9]+$ ]]; then
        # The other half of the ordering problem, and the only way to get our file in under a
        # profile that prefers what is already there: give up the slot, do not delete it.
        # deleteFiles=false means Radarr/Sonarr drop the RECORD and keep every byte, so the
        # source is still intact if anything below this fails.  Loud on purpose — a run that
        # had to unregister must be visible in the log even when it then succeeds.
        log "import: $app did not register $dest within $(( IMPORT_TRIES * IMPORT_POLL_SLEEP ))s while file #$oldid is registered"
        log "import: the profile ranks $(quality_token_for "$app" "$orig") — what $(basename "$orig") is registered as — above $(rewrite_token_of "$orig" "$dest"), what $(basename "$dest") reads as, so the replacement is not an upgrade while the original holds the slot"
        log "import: unregistering file #$oldid WITHOUT deleting its bytes (DELETE deleteFiles=false), then rescanning so $app can register $(basename "$dest"); $(basename "$orig") stays on disk either way"
        case $app in
          radarr) api radarr DELETE "/moviefile/$oldid?deleteFiles=false&addDeletedHistory=true" >/dev/null ;;
          sonarr) api sonarr DELETE "/episodefile/$oldid?deleteFiles=false&addDeletedHistory=true" >/dev/null ;;
        esac || log "import: WARNING $app would not unregister file #$oldid; the rewrite stays unregistered and nothing is deleted"
        rescan_command "$app" "$mid" \
          || log "import: WARNING $app would not accept the second rescan; $(basename "$dest") sits in the folder, $(basename "$orig") still on disk"
        rewrite_registration "$app" "$mid" "$dest" "$orig" "$before" newid || true
        rows=$LAST_ROWS
      else
        # Resume case (c): a previous run already unregistered the original — or it was never
        # registered at all — and our file is still not.  There is no record left to remove,
        # so this asks for another look and waits, changing nothing.
        log "import: $app has not registered $dest and does not list $(basename "$orig") either; rescanning item $mid again (no delete of any kind)"
        rescan_command "$app" "$mid" || true
        rewrite_registration "$app" "$mid" "$dest" "$orig" "$before" newid || true
        rows=$LAST_ROWS
      fi
    fi
  fi
  if [[ -z ${newid:-} ]]; then
    log "import FAILED: $app did not register $dest within $(( IMPORT_TRIES * IMPORT_POLL_SLEEP ))s, with and without the unregister path"
    log "  the rewrite sits in the library folder and $(basename "$orig") is untouched: $orig"
    return 1
  fi
  [[ $already -eq 1 ]] || log "import: registered as file #$newid"

  # ── phase 2: only now may the original's BYTES go. ───────────────────────
  # Ask the app again rather than trusting a listing from before the rescan: after an
  # unregister it no longer knows that id at all, and asking for deleteFiles=true on an id it
  # dropped is how this call used to look like it had removed something while it removed
  # nothing.  Where the app still holds the record, deleting through it takes the database row
  # with the bytes; where it does not, rm has to do it and the final rescan forgets the stub.
  oldid=$(jq -r --arg p "$orig" '[.[] | select(.path == $p)] | last // empty | .id' <<<"$rows")
  if [[ ! $oldid =~ ^[0-9]+$ ]]; then
    rows=$(file_rows_for "$app" "$mid") || rows='[]'
    oldid=$(jq -r --arg p "$orig" '[.[] | select(.path == $p)] | last // empty | .id' <<<"$rows")
  fi
  if [[ $oldid =~ ^[0-9]+$ ]]; then
    log "import: removing replaced file #$oldid $orig (deleteFiles=true: $app still has that record)"
    case $app in
      radarr) api radarr DELETE "/moviefile/$oldid?deleteFiles=true&addDeletedHistory=true" >/dev/null ;;
      sonarr) api sonarr DELETE "/episodefile/$oldid?deleteFiles=true&addDeletedHistory=true" >/dev/null ;;
    esac || log "import: WARNING $app would not remove file #$oldid; removing it directly"
  elif [[ -e $orig ]]; then
    # After an unregister (or on a resumed run where the app already forgot it) there is no id
    # left to ask with, so the bytes have to come off by hand and the rescan below clears the record.
    log "import: $(basename "$orig") is no longer registered with $app, so its bytes come off with rm"
  else
    log "import: note: $(basename "$orig") is neither registered nor on disk, so there is nothing to remove"
  fi
  # An accepted DELETE and the bytes disappearing are separate events — deleteFiles=true can
  # be refused, or honoured for the record while the file survives.  Fall back to rm (never
  # let it abort: a leftover is something to report, not a reason to stop mid-import), then
  # believe our own eyes rather than either call.
  if [[ -e $orig ]]; then rm -f -- "$orig" || true; fi
  rescan_command "$app" "$mid" || true          # so the app forgets the removed record too
  if [[ -e $orig ]]; then
    N_ORIG_LEFT=$(( N_ORIG_LEFT + 1 ))
    log "import: WARNING original still on disk after both removal attempts: $orig"
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
      fcoll= ; case $app in radarr) fcoll=moviefile ;; sonarr) fcoll=episodefile ;; esac
      got=
      # Same rule as the profile pin: an accepted PUT is not the file carrying that quality.
      # Used to log "stamped" unconditionally, i.e. a refusal printed its WARNING *and* the
      # success line right underneath it.  Read the record back and report what it reads.
      api "$app" PUT "/$fcoll/$newid" \
        "$(jq -nc --argjson q "$qid" --arg n "$QUALITY_NAME" '{quality: {quality: {id: $q, name: $n}, revision: 1}}')" >/dev/null \
        && got=$(item_json "$app" "$fcoll" "$newid" | jq -r '.quality.quality.name // empty' 2>/dev/null || true)
      if [[ $got == "$QUALITY_NAME" ]]; then
        log "import: stamped $QUALITY_NAME on file #$newid because QUALITY_MODE=override (the app reports it)"
      else
        log "import: WARNING quality $QUALITY_NAME could not be set on file #$newid (it reads ${got:-nothing})"
      fi
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
    if [[ ! -e "$vdir/$vsfx.$langpart" ]]; then
      # A sidecar that would not copy used to be silent: success logged itself, failure logged
      # nothing at all, and the tally still counted it because extraction had written it in
      # staging.  Say which sidecars actually moved beside the imported file.
      cp -p -- "$sfx" "$vdir/$vsfx.$langpart" \
        && log "subtitle: placed sidecar $vsfx.$langpart beside the imported file" \
        || log "import: WARNING subtitle: sidecar $vsfx.$langpart would not copy beside the imported file"
    fi
  done

  # Pin the item to the Normalised profile so the app never "upgrades" it back.  Best effort,
  # and deliberately last: by now the rewrite is registered and the original is gone, so this
  # cannot undo anything.  It logs its own verdict and counts N_PIN_FAILED itself — see
  # pin_item_to_profile for why it must never claim success on the strength of a PUT alone.
  pin_item_to_profile "$app" "$mid" || true

  # The last claim has to be a measured one.  What the poll above proved is that $app lists our
  # file and no longer lists the original; whether the original's bytes are off the disk is a
  # separate fact, checked separately.
  if [[ -e $orig ]]; then
    log "import: ok, $dest is registered as #$newid; $(basename "$orig") is unregistered but still on disk"
  else
    log "import: ok, $dest is registered as #$newid and $(basename "$orig") is gone"
  fi
}

# ── per-file driver ─────────────────────────────────────────────────────────
process_file() {
  local src=$1 app dir stem base staged tag discrc mate landed lmid lid
  app=$(route "$src"); [[ $app == none ]] && return 0
  N_SEEN=$(( N_SEEN + 1 ))
  SRC_CURRENT=$src           # set before any branch: verify() always has a reference
  # ── discs first, because for them the file on disk is not the thing being measured
  disc_reset; discrc=0
  if is_disc "$src"; then
    # Has this disc's feature already landed beside it?  Checked BEFORE anything tries to open the
    # image, so a converted disc still reads as done even when libbluray cannot re-open it — and a
    # 21 GB copy is never repeated just to prove the last one worked.  "Landed" is only HALF of
    # done, though: what counts is the file $app actually lists, not one that merely sits there.
    # Presence alone is what made movie 140 look finished forever — a conforming rewrite sat
    # unregistered while its own profile ranked the ISO above it, so every later run skipped the
    # folder, the disc stayed the library's copy of the film, and the job reported itself done.
    landed=
    lmid=$(item_id_for_path "$app" "$(dirname "$src")") || true
    while IFS= read -r mate; do
      [[ -n $mate ]] || continue
      conforming "$mate" || continue
      lid=
      if [[ -n ${lmid:-} && $lmid != null ]]; then
        lid=$(file_rows_for "$app" "$lmid" | jq -r --arg p "$mate" '[.[] | select(.path == $p)] | last // empty | .id' 2>/dev/null) || lid=
      fi
      if [[ $lid =~ ^[0-9]+$ ]]; then
        log "skip (conforming): $src (main feature already landed as $(basename "$mate"), registered with $app as file #$lid)"
        N_SKIP=$(( N_SKIP + 1 )); return 0
      fi
      # Conforming and on disk, but unregistered: this item is mid-import, not finished.
      landed=$mate
    done < <(disc_landed_mates "$src")
    if [[ -n ${landed:-} ]]; then
      log "import needed: $src — $(basename "$landed") is a conforming rewrite in the folder that $app does not list, so the disc still holds the slot; going straight to the import path with no new copy"
      if [[ $DRY_RUN -eq 1 ]]; then return 0; fi
      # A file that was never staged has no staging directory to clean up — and the folder it
      # sits in is the film's own, not ours to delete.
      if import_item "$app" "$landed" "" "$src"; then
        N_IMPORTED=$(( N_IMPORTED + 1 )); log "done: $src"
        # The staging directory the earlier run left behind is now provably redundant — the
        # rewrite it was made from is registered in the library — so it goes, keyed exactly the
        # way process_file keys it below.  Nothing else lives in there for this source.
        stem=$(basename "$src"); stem=${stem%.*}
        tag=$(printf '%s' "$src" | md5sum | cut -c1-8)
        if [[ -d "$STAGING_ROOT/$(split_of "$src")/$stem-$tag" ]]; then
          rm -rf -- "$STAGING_ROOT/$(split_of "$src")/$stem-$tag"
          log "staging: removed the leftover staging directory for $(basename "$src") now that its rewrite is registered"
        fi
      else
        N_FAILED=$(( N_FAILED + 1 ))
        log "import FAILED: $landed (in the library folder, still unregistered; nothing was deleted)"
      fi
      return 0
    fi
    disc_select "$src" || discrc=$?
    if (( discrc == 2 )); then   # no libbluray, no mount: an image nobody can open is not "non-conforming"
      log "skip (unsupported container): $src"; N_UNREADABLE=$(( N_UNREADABLE + 1 )); return 0
    elif (( discrc == 1 )); then
      log "skip (ambiguous disc titles): $src"; N_AMBIGUOUS=$(( N_AMBIGUOUS + 1 )); return 0
    fi
    # Measure the token BEFORE conforming() on a sibling swaps the facts cache, and from THIS
    # disc's selected title rather than from its file name: see disc_quality_token.
    facts "$src"; DISC_TOKEN=$(disc_quality_token)
    case $DISC_TOKEN in
      '') log "name: $(basename "$src") reports no usable picture height, so its rewrite lands with no quality token at all" >/dev/null ;;
      *)  log "name: $(basename "$src") is a disc IMAGE, not a disc: BR-DISK would claim a full remux this file is not, so it lands as $DISC_TOKEN — the token an Arr app reads off a stream-copied $(fact vheight)p bluray picture" >/dev/null ;;
    esac
  fi
  if conforming "$src"; then log "skip (conforming): $src"; N_SKIP=$(( N_SKIP + 1 )); return 0; fi
  if [[ $DRY_RUN -eq 1 ]]; then
    # A dry run that only says "would rewrite" is not actionable: name the reason,
    # because these are the numbers the whole policy is judged by.
    log "rewrite needed: $src"
    while IFS= read -r l; do [[ -n $l ]] && log "  why: $l"; done <<<"$(conformity_failures "$src")"
    return 0
  fi
  # The staging DIRECTORY keeps the source's own stem plus a hash of its full path (see
  # below), so a resume still finds it whatever the app records; only the FILE inside — and
  # therefore the library name, which import_item takes from it — carries the quality token.
  stem=$(basename "$src"); stem=${stem%.*}
  base=$(name_stem_for "$app" "$src")
  # Staging is keyed on the source path, not just the file stem.  Two different
  # titles can easily share a stem (Sample.mp4, trailer.mp4, or generic episode
  # names across shows), and with a shared staging directory one title's finished
  # encode gets reused — and then imported — as another's: the import call only
  # ever sees "<stem>.mp4" inside folderPath, so nothing else could tell them
  # apart.  The hash keeps each source its own folder while staying deterministic,
  # so a staged encode is still reusable across runs.
  tag=$(printf '%s' "$src" | md5sum | cut -c1-8)
  # Keyed on the ORIGINAL stem ($stem), not the tokened one: a resume must find the cached
  # rewrite even if the app's recorded quality — and so the file name inside — changed since.
  dir="$STAGING_ROOT/$(split_of "$src")/$stem-$tag"
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
    [[ -n $dir ]] && rm -rf -- "$dir"
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
        # The containers this tool has always taken, plus .mov and .flv — measured live on this box,
        # ffprobe reads both (Steal This Film II.720p.mov is h264 1280x720 + aac; Drop Dead Gorgeous
        # SDTV.flv is h264 528x384 + aac), so those two were never unreadable here, merely invisible
        # to this filter.  Two whole film folders sat in the library with no log line about them at
        # all, which is the real cost of an extension list nobody audits.
        *.mp4|*.mkv|*.m2ts|*.ts|*.avi|*.wmv|*.mpg|*.mpeg|*.mov|*.flv) process_file "$f" ;;
        # Optical disc images: read through libbluray, see disc_select.
        *.iso|*.img) process_file "$f" ;;
        # Extensions that look like video but nothing here has ever looked at.  Whatever ffprobe
        # makes of one, it gets SAID: a library that quietly holds titles nobody considered is the
        # failure mode this branch exists to kill.  The probe result is reported so the next reader
        # can decide whether to accept the container instead of re-probing by hand.
        *.webm|*.m4v|*.ogm|*.mk3d|*.divx|*.f4v|*.vob|*.evo|*.rec|*.3gp|*.asf|*.rm|*.rmvb|*.mp2|*.mpv|*.dat|*.ogv|*.swf)
          saw=''; saw=$($FP -v error -select_streams v:0 -show_entries stream=codec_name,width,height \
                           -of csv=p=0 "$f" 2>/dev/null | head -1) || saw=
          log "skip (unsupported container): $f"
          if [[ -n $saw ]]; then
            log "  why: ffprobe reads it as $saw but this extension is not in the accepted set, so nothing here rewrites it"
          else
            log "  why: ffprobe finds no video stream in it at all (not a container ffmpeg knows)"
          fi
          N_UNREADABLE=$(( N_UNREADABLE + 1 )) ;;
      esac
    done < <(find "$root" -type f -not -path '*/.*' -not -path '*/#*' -print0 | sort -z)
  done <<<"$LIBRARY_ROOTS"
  log "summary: $nroots root(s) walked; seen=$N_SEEN conforming=$N_SKIP rewritten=$N_REWRITTEN sidecars=$N_SIDE imported=$N_IMPORTED failed=$N_FAILED pin-failed=$N_PIN_FAILED original-left=$N_ORIG_LEFT unreadable=$N_UNREADABLE ambiguous=$N_AMBIGUOUS"
  log "=== media-normalise run end ==="
}

main "$@"
