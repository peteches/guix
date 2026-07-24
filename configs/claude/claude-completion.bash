# bash completion for the claude CLI.
#
# Option lists are hard-coded rather than scraped from `claude --help'
# since that would run the binary on every completion attempt. Re-check
# these against `claude --help' when claude-code is updated.

# Agent names from .claude/agents/*.md, project then user scope.
_claude_agents() {
    local d
    for d in ".claude/agents" "$HOME/.claude/agents"; do
        [ -d "$d" ] || continue
        find "$d" -maxdepth 1 -name '*.md' -printf '%f\n' 2>/dev/null | sed 's/\.md$//'
    done | sort -u
}

# COMPREPLY from a word list, without the SC2207 splitting hazard.
_claude_words() {
    mapfile -t COMPREPLY < <(compgen -W "$1" -- "$cur")
}

# Fall back to plain compgen where bash-completion's _filedir is absent,
# so this script is usable on its own.
_claude_filedir() {
    if declare -F _filedir >/dev/null 2>&1; then
        _filedir "$@"
    elif [ "${1:-}" = "-d" ]; then
        mapfile -t COMPREPLY < <(compgen -d -- "$cur")
    else
        mapfile -t COMPREPLY < <(compgen -f -- "$cur")
    fi
}

_claude_cli_opts="
    --add-dir --agent --agents --allow-dangerously-skip-permissions
    --allowed-tools --allowedTools --append-system-prompt
    --ax-screen-reader --background --bare --betas --bg --brief
    -c --continue --chrome --no-chrome
    -d --debug --debug-file --dangerously-skip-permissions
    --disable-slash-commands --disallowed-tools --disallowedTools
    --effort --exclude-dynamic-system-prompt-sections
    --fallback-model --file --fork-session --forward-subagent-text
    --from-pr -h --help --ide --include-hook-events
    --include-partial-messages --input-format --json-schema
    --max-budget-usd --mcp-config --model -n --name
    --no-session-persistence --output-format -p --print
    --permission-mode --plugin-dir --plugin-url --prompt-suggestions
    -r --resume --remote-control --remote-control-session-name-prefix
    --replay-user-messages --safe-mode --session-id --settings
    --setting-sources --strict-mcp-config --system-prompt --tmux
    --tools -v --verbose --version -w --worktree
"

_claude_cli_cmds="
    agents auth auto-mode doctor gateway install mcp plugin plugins
    project setup-token ultrareview update upgrade
"

_claude() {
    local cur prev
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    # COMP_WORDBREAKS contains `=', so --opt=val arrives as three words
    # (--opt, =, val).  Normalise so --opt=val and --opt val take the
    # same code path below.
    if [[ $cur == "=" ]]; then
        cur=""
    elif [[ $prev == "=" ]]; then
        prev="${COMP_WORDS[COMP_CWORD-2]}"
    fi

    case "$prev" in
        --model|--fallback-model)
            _claude_words "fable opus sonnet haiku
                           claude-fable-5 claude-opus-4-8
                           claude-sonnet-5
                           claude-haiku-4-5-20251001"
            return ;;
        --permission-mode)
            _claude_words "acceptEdits auto bypassPermissions
                           manual dontAsk plan"
            return ;;
        --output-format)
            _claude_words "text json stream-json"
            return ;;
        --input-format)
            _claude_words "text stream-json"
            return ;;
        --effort)
            _claude_words "low medium high xhigh max"
            return ;;
        --setting-sources)
            _claude_words "user project local"
            return ;;
        --agent)
            _claude_words "$(_claude_agents)"
            return ;;
        --settings|--debug-file|--mcp-config|--file|--json-schema)
            _claude_filedir
            return ;;
        --add-dir|--plugin-dir)
            _claude_filedir -d
            return ;;
    esac

    if [[ $cur == -* ]]; then
        _claude_words "$_claude_cli_opts"
    elif (( COMP_CWORD == 1 )); then
        # Subcommands only in first position; past that a bare word is
        # the prompt, which nothing can usefully complete.
        _claude_words "$_claude_cli_cmds"
    fi
}

complete -F _claude claude
