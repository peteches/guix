#!/usr/bin/env bash
set -euo pipefail

# bump-channel.sh <commit-sha> — update the critical-grind channel's pinned
# commit everywhere it's actually built from.
#
# Two files carry this pin, not one — a real gap this script used to claim
# didn't exist:
#   - peteches/channels/critical-grind.scm       — the live channel entry.
#   - peteches/channels/deploy-critical-grind.scm — its header says
#     "duplicates every entry ... keep in sync by hand", and this is the file
#     ci/tasks/critical-grind-deploy.yml's `guix pull -C` actually builds
#     from. Only bumping critical-grind.scm left deploy-critical-grind.scm
#     frozen at whatever commit it was created with (15e0724..., 2026-07-29)
#     indefinitely: every deploy build kept logging the right APP_COMMIT
#     (read from critical-grind.scm, cosmetic only) while silently building
#     and shipping that stale July commit instead, generation after
#     identical generation, with no error at any layer.
#
# deploy-critical-grind.scm holds six channels' worth of (commit "...")
# lines, so its substitution is scoped to the critical-grind channel's own
# block (the '(name 'critical-grind)' line through the next '(commit'
# line) rather than a blind global replace, which would also clobber the
# other five channels' unrelated pins.
#
# Used by ci/tasks/critical-grind-bump-channel.yml; also safe to run by hand.

if [ $# -ne 1 ]; then
  echo "usage: $0 <commit-sha>" >&2
  exit 1
fi

COMMIT="$1"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
CHANNEL_FILE="$REPO_ROOT/peteches/channels/critical-grind.scm"
DEPLOY_CHANNEL_FILE="$REPO_ROOT/peteches/channels/deploy-critical-grind.scm"

if [[ ! "$COMMIT" =~ ^[0-9a-f]{40}$ ]]; then
  echo "error: '$COMMIT' does not look like a full 40-character git SHA" >&2
  exit 1
fi

if [ ! -f "$CHANNEL_FILE" ]; then
  echo "error: $CHANNEL_FILE not found" >&2
  exit 1
fi
if [ ! -f "$DEPLOY_CHANNEL_FILE" ]; then
  echo "error: $DEPLOY_CHANNEL_FILE not found" >&2
  exit 1
fi

sed -i -E "s/\(commit \"[0-9a-f]{40}\"\)/(commit \"$COMMIT\")/" "$CHANNEL_FILE"

sed -i -E "/\(name 'critical-grind\)/,/\(commit/ s/\(commit \"[0-9a-f]{40}\"\)/(commit \"$COMMIT\")/" \
  "$DEPLOY_CHANNEL_FILE"

if ! grep -q "(commit \"$COMMIT\")" "$CHANNEL_FILE"; then
  echo "error: commit substitution did not take effect in $CHANNEL_FILE" >&2
  exit 1
fi
if ! grep -A3 "(name 'critical-grind)" "$DEPLOY_CHANNEL_FILE" | grep -q "(commit \"$COMMIT\")"; then
  echo "error: commit substitution did not take effect in $DEPLOY_CHANNEL_FILE" >&2
  exit 1
fi

echo "bumped critical-grind channel to $COMMIT"
