#!/usr/bin/env bash
set -euo pipefail

# bump-channel.sh <commit-sha> — update the critical-grind channel's pinned
# commit in peteches/channels/critical-grind.scm.  That is the only file that
# names this channel's commit: base.scm/manual.scm no longer reference it at
# all (see the channel-split note at the top of base.scm), so there is
# exactly one file to edit and no risk of missing one, unlike guix's own
# three-file channel convention this repo otherwise follows.
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

if [[ ! "$COMMIT" =~ ^[0-9a-f]{40}$ ]]; then
  echo "error: '$COMMIT' does not look like a full 40-character git SHA" >&2
  exit 1
fi

if [ ! -f "$CHANNEL_FILE" ]; then
  echo "error: $CHANNEL_FILE not found" >&2
  exit 1
fi

sed -i -E "s/\(commit \"[0-9a-f]{40}\"\)/(commit \"$COMMIT\")/" "$CHANNEL_FILE"

if ! grep -q "(commit \"$COMMIT\")" "$CHANNEL_FILE"; then
  echo "error: commit substitution did not take effect" >&2
  exit 1
fi

echo "bumped critical-grind channel to $COMMIT"
