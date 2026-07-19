# bash completion for the claude-container wrapper.
#
# Completes the wrapper's own options up to `--', and the claude-code
# CLI's options/subcommands after it.  The wrapper also forwards bare
# (unrecognised) arguments to claude, but this script deliberately does
# not offer claude flags before `--': the two share option names
# (--worktree most notably) and guessing which side the user means
# produces worse completions than the explicit split.
#
# Option lists are hard-coded rather than scraped from `claude --help'.
# The real claude binary lives inside the container and is not on the
# host PATH, so at completion time `claude --help' would print the
# *wrapper's* usage.  Re-check these against `claude --help' run inside
# a container when claude-code is updated.

# Session directory names, matching the wrapper's own --list-sessions.
_claude_container_sessions() {
    local dir="${CLAUDE_CONTAINER_SESSIONS_DIR:-$HOME/.claude-sessions}"
    [ -d "$dir" ] || return 0
    find "$dir" -mindepth 1 -maxdepth 1 -type d -printf '%f\n' 2>/dev/null | sort -u
}

# Local branch names, for `--worktree BRANCH'.  Silent outside a repo.
_claude_container_branches() {
    git branch --format='%(refname:short)' 2>/dev/null
}

# Agent names from .claude/agents/*.md, project then user scope.
_claude_container_agents() {
    local d
    for d in ".claude/agents" "$HOME/.claude/agents"; do
        [ -d "$d" ] || continue
        find "$d" -maxdepth 1 -name '*.md' -printf '%f\n' 2>/dev/null | sed 's/\.md$//'
    done | sort -u
}

# COMPREPLY from a word list, without the SC2207 splitting hazard.
_claude_container_words() {
    mapfile -t COMPREPLY < <(compgen -W "$1" -- "$cur")
}

# Fall back to plain compgen where bash-completion's _filedir is absent,
# so this script is usable on its own.
_claude_container_filedir() {
    if declare -F _filedir >/dev/null 2>&1; then
        _filedir "$@"
    elif [ "${1:-}" = "-d" ]; then
        mapfile -t COMPREPLY < <(compgen -d -- "$cur")
    else
        mapfile -t COMPREPLY < <(compgen -f -- "$cur")
    fi
}

_claude_container_wrapper_opts="
    --session --migrate --share --expose
    --worktree --no-worktree --keep-worktree
    --no-anvil --nesting --guix --no-nesting
    --shell --rm --ephemeral --list-sessions
    -h --help --
"

_claude_container_cli_opts="
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

_claude_container_cli_cmds="
    agents auth auto-mode doctor gateway install mcp plugin plugins
    project setup-token ultrareview update upgrade
"

# Completions for the claude CLI half (everything after `--').
_claude_container_cli() {
    case "$prev" in
        --model|--fallback-model)
            _claude_container_words "fable opus sonnet haiku
                                     claude-fable-5 claude-opus-4-8
                                     claude-sonnet-5
                                     claude-haiku-4-5-20251001"
            return ;;
        --permission-mode)
            _claude_container_words "acceptEdits auto bypassPermissions
                                     manual dontAsk plan"
            return ;;
        --output-format)
            _claude_container_words "text json stream-json"
            return ;;
        --input-format)
            _claude_container_words "text stream-json"
            return ;;
        --effort)
            _claude_container_words "low medium high xhigh max"
            return ;;
        --setting-sources)
            _claude_container_words "user project local"
            return ;;
        --agent)
            _claude_container_words "$(_claude_container_agents)"
            return ;;
        --settings|--debug-file|--mcp-config|--file|--json-schema)
            _claude_container_filedir
            return ;;
        --add-dir|--plugin-dir)
            _claude_container_filedir -d
            return ;;
    esac

    if [[ $cur == -* ]]; then
        _claude_container_words "$_claude_container_cli_opts"
    elif (( COMP_CWORD == dashdash + 1 )); then
        # Subcommands only in first position after `--'; past that a bare
        # word is the prompt, which nothing can usefully complete.
        _claude_container_words "$_claude_container_cli_cmds"
    fi
}

_claude_container() {
    local cur prev i dashdash=-1
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    # COMP_WORDBREAKS contains `=', so --session=foo arrives as three
    # words (--session, =, foo).  Normalise so --opt=val and --opt val
    # take the same code path below.
    if [[ $cur == "=" ]]; then
        cur=""
    elif [[ $prev == "=" ]]; then
        prev="${COMP_WORDS[COMP_CWORD-2]}"
    fi

    for ((i = 1; i < COMP_CWORD; i++)); do
        if [[ ${COMP_WORDS[i]} == "--" ]]; then
            dashdash=$i
            break
        fi
    done

    if (( dashdash >= 0 )); then
        _claude_container_cli
        return
    fi

    case "$prev" in
        --session)
            _claude_container_words "$(_claude_container_sessions)"
            return ;;
        --migrate)
            _claude_container_words "y n"
            return ;;
        --share|--expose)
            # PATH or PATH=DEST; complete the PATH half only.
            _claude_container_filedir
            return ;;
        --worktree)
            # Optional argument: offer branches, but keep wrapper flags
            # available since `--worktree --shell' is valid.
            _claude_container_words "$(_claude_container_branches)
                                     $_claude_container_wrapper_opts"
            return ;;
    esac

    _claude_container_words "$_claude_container_wrapper_opts"
}

complete -F _claude_container claude
