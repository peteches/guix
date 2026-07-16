#!/usr/bin/env bash
input=$(cat)
cwd=$(echo "$input" | jq -r '.workspace.current_dir // empty')
[ -z "$cwd" ] && cwd=$(pwd)

ctx_suffix=""
used=$(echo "$input" | jq -r '.context_window.used_percentage // empty')
[ -n "$used" ] && ctx_suffix=$(printf " [ctx: %.0f%%]" "$used")

cost_suffix=""
cost=$(echo "$input" | jq -r '.cost.total_cost_usd // empty')
[ -n "$cost" ] && cost_suffix=$(printf " [~\$%.4f est.]" "$cost")

rel_time() {
    local delta=$(( $1 - $(date +%s) ))
    if [ "$delta" -le 0 ]; then
        printf "now"
    elif [ "$delta" -lt 3600 ]; then
        printf "%dm" $(( delta / 60 ))
    elif [ "$delta" -lt 86400 ]; then
        printf "%dh%dm" $(( delta / 3600 )) $(( (delta % 3600) / 60 ))
    else
        printf "%dd%dh" $(( delta / 86400 )) $(( (delta % 86400) / 3600 ))
    fi
}

rate_suffix=""
five_h=$(echo "$input" | jq -r '.rate_limits.five_hour.used_percentage // empty')
five_h_reset=$(echo "$input" | jq -r '.rate_limits.five_hour.resets_at // empty')
seven_d=$(echo "$input" | jq -r '.rate_limits.seven_day.used_percentage // empty')
seven_d_reset=$(echo "$input" | jq -r '.rate_limits.seven_day.resets_at // empty')

five_h_str=""
seven_d_str=""
[ -n "$five_h" ] && {
    five_h_str=$(printf "5h: %.0f%%" "$five_h")
    [ -n "$five_h_reset" ] && five_h_str="$five_h_str (resets in $(rel_time "$five_h_reset"))"
}
[ -n "$seven_d" ] && {
    seven_d_str=$(printf "7d: %.0f%%" "$seven_d")
    [ -n "$seven_d_reset" ] && seven_d_str="$seven_d_str (resets in $(rel_time "$seven_d_reset"))"
}

[ -n "$five_h_str" ] || [ -n "$seven_d_str" ] && \
    rate_suffix=" [${five_h_str}${five_h_str:+${seven_d_str:+ }}${seven_d_str}]"

# Session name, exported by containers/claude.scm as CLAUDE_SESSION.
# Blank when running claude outside the container wrapper.
session_suffix=""
[ -n "${CLAUDE_SESSION:-}" ] && session_suffix="/${CLAUDE_SESSION}"

printf '%s@%s%s %s%s%s%s%s' \
    "$(whoami)" "$(hostname -s)" "$session_suffix" "$cwd" \
    "${GUIX_ENVIRONMENT:+ [env]}" \
    "$ctx_suffix" "$cost_suffix" "$rate_suffix"
