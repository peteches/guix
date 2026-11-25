#!/usr/bin/env bash
set -euo pipefail

# bump-channel.sh <commit-sha> — update the critical-grind channel's pinned
# commit.
#
;;; This is the sole pin for the critical-grind channel.  The old setup kept a
;;; duplicate in deploy-critical-grind.scm which caused a real bug: after
;;; bumping only critical-grind.scm, every CI deploy build would silently
;;; continue using the stale July 2026 commit from the un-updated deploy file,
;;; with no error at any layer.
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

sed -i -E "s/(commit \")([0-9a-f]{40})(\")/\1${COMMIT}\3/" "$CHANNEL_FILE"

if ! grep -q "(commit \"$COMMIT\")" "$CHANNEL_FILE"; then
    echo "error: commit substitution did not take effect in $CHANNEL_FILE" >&2
    exit 1
fi

echo "bumped critical-grind channel to $COMMIT"
