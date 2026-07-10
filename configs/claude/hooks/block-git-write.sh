#!/usr/bin/env bash
# Claude Code PreToolUse hook — block git add/commit/push.
# The user reviews and commits all changes themselves.

input="${CLAUDE_TOOL_INPUT:-}"
if [[ -z "$input" ]]; then exit 0; fi

cmd=$(python3 -c "import sys, json; print(json.loads(sys.argv[1]).get('command', ''))" "$input" 2>/dev/null)

if echo "$cmd" | grep -qE 'git (add|commit|push)'; then
  python3 -c "
import json
print(json.dumps({
  'decision': 'block',
  'reason': 'git add/commit/push is not permitted — the user reviews and commits all changes themselves.'
}))
"
fi
