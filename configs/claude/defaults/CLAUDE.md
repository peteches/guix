# Global Claude Code Instructions

## Secret Handling — MANDATORY

**Never print, echo, or display secret or key material in any shell
command output.** This conversation is recorded by Anthropic. Anything
that appears in a tool result is part of that record and must be
treated as permanently exposed.

This applies to **all** key material without exception:
- Private keys (SSH, age, GPG)
- Passwords and passphrases
- **Public keys** — even public keys reveal which identity/host owns
  them and enable correlation attacks (e.g. against SOPS recipients or
  SSH `authorized_keys`)
- Age public keys — expose which recipients can decrypt which secrets
- SSH host keys — fingerprints help attackers impersonate hosts
- IP addresses of newly provisioned machines before they are publicly
  registered
- Any value read out of an encrypted secrets file (SOPS,
  sealed-secrets, `pass`, `.env`, etc.)

**Rules:**
1. Secrets must live exclusively in shell variables or `/dev/shm`
   files. Never in `/tmp`.
2. Shell commands must **not** `echo`, `cat`, `print`, or otherwise
   output key material to stdout or stderr. Status messages like
   `echo "Key saved."` are fine; `echo "$KEY"` is not.
3. When a value is needed across steps (age key, host key, token),
   store it in a `/dev/shm` file and read it back silently — do not
   echo it as confirmation.
4. Use `shred -u` to destroy `/dev/shm` files as soon as they are no
   longer needed.
5. If a secret has appeared in conversation output, treat it as
   compromised and rotate it.

## Anvil MCP — the in-container Emacs bridge

You are running inside the `claude` container (defined in
`containers/claude.scm`).  On launch the container's startup
script (`claude-container-startup.sh`) brings up a headless
`emacs --daemon` with [anvil.el](https://github.com/zawatton/anvil.el)
loaded from the Guix `emacs-anvil` package.  The daemon exposes
the Anvil MCP server so you get low-token file/org/eval/worker
tools instead of round-tripping whole files through
Read/Edit/Write.

Key facts about this setup:

- `emacs-anvil` is a first-class Guix package — no straight.el,
  no network at container startup, no cold-start wait.  The
  emacs config lives at `~/.config/emacs/init.el` (session-
  persistent) and just does `(require 'anvil) (anvil-enable)`.
- On every container launch the startup script rewrites the
  `mcpServers.anvil` and `mcpServers.anvil-emacs-eval` entries
  in `~/.claude.json` with container-resolved paths
  (`command -v bash` and `command -v anvil-stdio.sh`), so
  a host `guix gc` cannot orphan the seeded config.
- Session-persistent daemon log at
  `~/.config/emacs/daemon.log` — check it when anvil tools are
  missing.
- If a tool call fails with `MCP error: daemon returned empty
  response` or `can't find socket`, the daemon is still coming
  up or has died — retry once, then fall back to built-in
  Read/Edit/Write for the immediate task.

**Always prefer Anvil tools over the built-in file tools where
they apply.**  They ship only the delta and avoid full-file
reads.

## File editing

- `anvil-file-batch` — 3+ edits to the same file (collapse into
  one call).
- `anvil-file-replace-string` / `anvil-file-replace-regexp` —
  pinpoint replacement; no full-file read required.
- `anvil-file-insert-at-line` / `anvil-file-delete-lines` /
  `anvil-file-append` — localized line-level operations.

Use the built-in `Edit` only for small one-off changes.  For 3
or more edits to the same file, always use `anvil-file-batch`.

## org-mode

For section moves, refile, splits, or reading a single heading
from a large org file, use `anvil-org-*` tools instead of
Read+Write.  They are 10–20× cheaper in tokens.

- `anvil-org-read-headline` — read a single subtree.
- `anvil-org-read-outline` — outline view without bodies.
- `anvil-org-edit-body` / `anvil-org-rename-headline` /
  `anvil-org-update-todo-state` — targeted org edits.

## Heavy operations — worker dispatch

Long-running Emacs ops (large tangles, byte-compile, multi-MB
org scans, full-tree searches) must not run on the main daemon
— they block every other tool call while the container's single
emacs instance is busy.  Dispatch them through the worker pool
instead.

- Prefer `anvil-worker-call` over raw `eval` for anything that
  may exceed ~1s.
- If a call to the main `anvil` MCP hangs for several seconds,
  that's the signal you should have gone through the worker.

## Context and output compression

The `context` optional module is loaded, so both compression
tools below are available.  Use them when command output or
retrieved context is long.

- `context-compress` for non-shell text: API JSON, RAG snippets,
  web/article extracts, logs from another tool, diffs, or code
  excerpts.  Set `store=true` when the raw text may be needed
  later; recover it with `context-retrieve` and the returned
  `ccr-id`.
- `shell-run` for shell commands whose stdout can be filtered
  automatically.  It returns compressed stdout plus a `tee-id`;
  recover the raw output with `shell-tee-get` only when the
  compressed view is insufficient.
- `context-stats` / `shell-gain` show actual savings — inspect
  rather than guess whether compression is helping.

Do not use compressed views as the only source of truth for
legal, financial, safety-critical, or exact numeric work.
Retrieve the raw context before making claims that depend on
exact wording or values.

## Guix service and package research

When working on any Guix system or home configuration — writing a new service, adding a package, or researching an existing service definition — use the CLI lookup commands to find authoritative information before editing files:

- `guix system search <term>` — find system services
- `guix home search <term>` — find home services
- `guix package -s <term>` — find packages

Pipe results through `recsel` to filter by field (e.g. `-e 'name ~ "foo"' -p name,location`). Use the `location` field to find the source `.scm` file on disk by scanning `GUILE_LOAD_PATH`. See the `/guix-lookup` skill for the full workflow.
