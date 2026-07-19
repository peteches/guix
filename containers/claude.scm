(define-module (containers claude)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (peteches repository)
  #:use-module (peteches packages comfyui-mcp)
  #:export (claude-container-manifest
            claude-container
            emacs-anvil
            comfyui-mcp))

;; Package specifications made available inside the container.
;; claude-code comes from the peteches channel; the rest are vanilla Guix.
;; `emacs-no-x' hosts the in-container Anvil MCP server.
;; `perl' is used by anvil-stdio.sh as an alarm-timeout fallback when
;; GNU `timeout' is absent.
;;
;; Agentic-coding staples: shellcheck, moreutils, yq, bind:utils (dig),
;; netcat-openbsd, socat, procps, psmisc, make, openssl, recutils.
(define %claude-container-specs
  '("claude-code"
    "bash"
    "bind:utils"
    "bzip2"
    "coreutils"
    "curl"
    "diffutils"
    "emacs-no-x"
    "fd"
    "file"
    "findutils"
    "gawk"
    "git"
    "glibc-locales"
    "gnupg"
    "grep"
    "guile"
    "guix"
    "gzip"
    "jq"
    "less"
    "make"
    "mcp-grafana"
    "mcp-outline"
    "moreutils"
    "netcat-openbsd"
    "node"
    "nss-certs"
    "openssh"
    "openssl"
    "perl"
    "plane-mcp-server"
    "procps"
    "psmisc"
    "python-wrapper"
    "recutils"
    "ripgrep"
    "sed"
    "shellcheck"
    "socat"
    "tar"
    "tree"
    "unzip"
    "util-linux"
    "which"
    "wget"
    "xz"
    "yq"))

;; anvil.el packaged as a first-class Guix package.  Replaces the earlier
;; straight.el bootstrap so first-launch is instant and the container has
;; no network dependency during startup.  Byte-compilation is deferred to
;; Emacs at load time to avoid failing on optional-integration modules.
(define anvil-commit "574568a95a2bd8fceca6c9cd3bec0f94ecf0e6a9")
(define anvil-revision "1")

(define-public emacs-anvil
  (package
    (name "emacs-anvil")
    (version (git-version "1.3.0" anvil-revision anvil-commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zawatton/anvil.el")
             (commit anvil-commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b766fv7fqnpx3qxlcdj272dr59626g4k36yxpzlvj0pm5j1iz6g"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils) (ice-9 ftw))
      #:builder
      #~(begin
          (use-modules (guix build utils) (ice-9 ftw))
          (let* ((src       #$source)
                 (out       #$output)
                 (site-lisp (string-append out "/share/emacs/site-lisp/anvil"))
                 (bin       (string-append out "/bin")))
            (mkdir-p site-lisp)
            (mkdir-p bin)
            (for-each
             (lambda (f)
               (copy-file (string-append src "/" f)
                          (string-append site-lisp "/" f)))
             (scandir src
                      (lambda (n)
                        (and (string-suffix? ".el" n)
                             (not (string-prefix? "." n))))))
            (let* ((stdio (string-append src "/anvil-stdio.sh"))
                   (dest  (string-append bin "/anvil-stdio.sh")))
              (copy-file stdio dest)
              (chmod dest #o755))))))
    (synopsis "Emacs MCP server bridging LLM agents to Emacs")
    (description
     "Anvil turns Emacs into an AI-ready workbench via the Model Context
Protocol (MCP).  It exposes file editing, org-mode operations, system
inspection, and Elisp evaluation as MCP tools that any LLM agent can
call over stdio.  This package installs the elisp modules and the
@command{anvil-stdio.sh} launcher.")
    (home-page "https://github.com/zawatton/anvil.el")
    (license license:gpl3+)))

;; The comfyui-mcp MCP server (https://github.com/artokun/comfyui-mcp),
;; from the peteches channel.
;;
;; This was previously a trivial wrapper that exec'd `npx -y
;; comfyui-mcp@latest', which never worked here: comfyui-mcp depends on
;; two native addons (better-sqlite3 and sharp), npx builds them from
;; source on first use, and the container ships no C compiler — so npx
;; failed with no diagnostics and the MCP server merely showed as
;; "failed to connect".  The channel package builds both addons at
;; package-build time instead.
(define comfyui-mcp node-comfyui-mcp)

(define %spec-list-str
  (string-join (map (lambda (s) (string-append "\"" s "\""))
                    %claude-container-specs)
               "\n   "))

;; The container manifest merges the vanilla-Guix specification list
;; with store references to the `emacs-anvil' and `comfyui-mcp' wrapper
;; packages.  `mixed-text-file' substitutes each package with its
;; /gnu/store path at build time, so the resulting Scheme file needs no
;; module lookups.
(define claude-container-manifest
  (mixed-text-file
   "claude-container-manifest.scm"
   "(use-modules (guix profiles))\n"
   "(concatenate-manifests\n"
   " (list (specifications->manifest\n"
   "        '(" %spec-list-str "))\n"
   "       (manifest\n"
   "        (list (manifest-entry\n"
   "               (name \"emacs-anvil\")\n"
   "               (version \"" (package-version emacs-anvil) "\")\n"
   "               (item \"" emacs-anvil "\"))\n"
   "              (manifest-entry\n"
   "               (name \"comfyui-mcp\")\n"
   "               (version \"" (package-version comfyui-mcp) "\")\n"
   "               (item \"" comfyui-mcp "\"))))))\n"))

;; Minimal Emacs init.  emacs-anvil is on the load-path via
;; site-start.el, so `(require 'anvil)' just works.  Byte-compilation
;; is deferred to load time, keeping startup network-free and fast.
(define claude-container-anvil-init
  (mixed-text-file
   "anvil-init.el"
   ";;; init.el --- Anvil bootstrap for the claude container -*- lexical-binding: t; -*-\n"
   "\n"
   "(setq anvil-modules\n"
   "      '(worker eval file fs org text emacs elisp git proc host data\n"
   "               clipboard net http))\n"
   "\n"
   "(defvar anvil-optional-modules nil)\n"
   "(dolist (m '(context orchestrator memory))\n"
   "  (add-to-list 'anvil-optional-modules m))\n"
   "\n"
   "(require 'anvil)\n"
   "(when (fboundp 'anvil-enable)\n"
   "  (anvil-enable))\n"
   "\n"
   "(provide 'init)\n"
   ";;; init.el ends here\n"))

;; Runs as PID 1 inside `guix shell --container'.  Rewrites
;; ~/.claude.json's anvil MCP entries with container-resolved paths,
;; starts the emacs daemon (unless --no-anvil), sources per-session
;; hooks, then execs claude (or bash under --shell).
(define claude-container-startup
  (mixed-text-file
   "claude-container-startup.sh"
   "#!" bash-minimal "/bin/bash\n"
   "set -eu\n"
   "set -o pipefail\n"
   "\n"
   "log() { echo \"[claude-container] $*\" >&2; }\n"
   "\n"
   "# Neutralize Claude Code's `grep()' / `find()' shell functions.\n"
   "# claude-code injects them into its shells to reroute through\n"
   "# `claude -G ...' (ugrep mode) / `claude -S ...' (bfs mode).  Both\n"
   "# resolve the binary at call time as:\n"
   "#\n"
   "#   _cc_bin=\"${CLAUDE_CODE_EXECPATH:-}\"\n"
   "#   [[ -x $_cc_bin ]] || _cc_bin=~/.local/bin/claude\n"
   "#   [[ ! -x $_cc_bin ]] && { command grep \"$@\"; return; }   # fallback\n"
   "#   (exec -a ugrep \"$_cc_bin\" -G ... \"$@\")\n"
   "#\n"
   "# The Guix claude-code package's bin/claude is a *shell script* that\n"
   "# execs the glibc loader:\n"
   "#\n"
   "#   exec .../ld-linux-x86-64.so.2 --library-path ... .../claude-bin/claude \"$@\"\n"
   "#\n"
   "# so /proc/self/exe -- and hence CLAUDE_CODE_EXECPATH -- is the LOADER,\n"
   "# not claude.  The wrappers then run `ld.so -G ...', and ld.so tries to\n"
   "# load \"-G\" as a shared object:\n"
   "#\n"
   "#   -G: error while loading shared libraries: -G: cannot open shared\n"
   "#   object file: No such file or directory\n"
   "#\n"
   "# The fallback never fires because ld.so *is* executable, so the -x test\n"
   "# passes.  ugrep mode cannot be made to work under this packaging: the\n"
   "# wrapper script also destroys argv[0], and this claude-code rejects a\n"
   "# bare `-G' (\"error: unknown option '-G'\").  Fixing that needs the\n"
   "# package to patchelf the binary instead of wrapping it -- that lives in\n"
   "# the external peteches channel, not here.\n"
   "#\n"
   "# So force the fallback by pointing CLAUDE_CODE_EXECPATH at a\n"
   "# non-existent path.  The wrappers then call the real grep/find.\n"
   "#\n"
   "# `unset -f' alone is NOT enough and is kept only for shells that never\n"
   "# source a snapshot: the bash tool runs `bash -c 'source <snapshot>; cmd'\n"
   "# and ~/.claude/shell-snapshots/snapshot-bash-*.sh RE-DEFINES both\n"
   "# functions after BASH_ENV has already run.  The snapshot only re-exports\n"
   "# PATH, never CLAUDE_CODE_EXECPATH, so the export below survives it.\n"
   "mkdir -p \"$HOME\"\n"
   "cat > \"$HOME/.claude-container-bashrc\" <<'EOF'\n"
   "unset -f grep 2>/dev/null || true\n"
   "unset -f find 2>/dev/null || true\n"
   "export CLAUDE_CODE_EXECPATH=/nonexistent/claude-code-ugrep-disabled\n"
   "EOF\n"
   "export BASH_ENV=\"$HOME/.claude-container-bashrc\"\n"
   "\n"
   "# With --nesting, prefer the host's pulled guix: it is the only one that\n"
   "# can resolve channel modules ((nongnu ...), (sops ...), (guix-science\n"
   "# ...)), which the plain guix in the container manifest cannot.\n"
   "#\n"
   "# This must happen HERE, before `exec claude', rather than from BASH_ENV:\n"
   "# claude snapshots its environment into\n"
   "# ~/.claude/shell-snapshots/snapshot-bash-*.sh, and that snapshot ends\n"
   "# with an unconditional `export PATH=...'.  Every bash tool call sources\n"
   "# it AFTER BASH_ENV has run, so a PATH set in BASH_ENV would be\n"
   "# overwritten.  Setting it before claude starts gets it captured INTO the\n"
   "# snapshot instead.\n"
   "if [ -n \"${CLAUDE_CONTAINER_GUIX_CURRENT:-}\" ] \\\n"
   "   && [ -x \"${CLAUDE_CONTAINER_GUIX_CURRENT}/bin/guix\" ]; then\n"
   "  export PATH=\"${CLAUDE_CONTAINER_GUIX_CURRENT}/bin:$PATH\"\n"
   "  log \"guix nesting: using $(command -v guix)\"\n"
   "fi\n"
   "\n"
   "# Per-session env file: auto-exported KEY=VALUE lines.\n"
   "if [ -f \"$HOME/.claude-session/env\" ]; then\n"
   "  set -a\n"
   "  # shellcheck disable=SC1091\n"
   "  . \"$HOME/.claude-session/env\"\n"
   "  set +a\n"
   "fi\n"
   "\n"
   ;; Decrypted per-session secrets.  The wrapper runs `sops -d' on the
   ;; host and drops the plaintext into a host tmpfs file exposed here
   ;; read-only; it is never written to the session dir or the store,
   ;; and the wrapper shreds it on exit.  Sourced after the plaintext
   ;; env so a secret may override a placeholder set there.\n"
   "# Decrypted per-session secrets (sops), exposed read-only from a\n"
   "# host tmpfs by the wrapper.  Never persisted to the session dir.\n"
   "if [ -f \"$HOME/.claude-session-secrets\" ]; then\n"
   "  set -a\n"
   "  # shellcheck disable=SC1091\n"
   "  . \"$HOME/.claude-session-secrets\"\n"
   "  set +a\n"
   "fi\n"
   "\n"
   ;; Default Grafana endpoint for the homelab.  The grafana VM inherits
   ;; the base firewall, which opens only 22/9100/ICMP — port 3000 is
   ;; NOT reachable on the LAN — so this deliberately points at the
   ;; Tailscale-fronted Caddy vhost rather than 192.168.51.188:3000.
   ;;
   ;; A session `env' file (sourced above) or a preserved host
   ;; GRAFANA_URL still wins; this is only the fallback.
   ;;
   ;; Note `-' rather than `:-': that substitutes only when the variable
   ;; is *unset*, so a session can opt out of the Grafana MCP server
   ;; entirely with a bare `GRAFANA_URL=' line in its `env' file, which
   ;; register_grafana_mcp then treats as "remove the entry".  With `:-'
   ;; an explicit empty value would silently get the default back.
   "# Default Grafana endpoint (Tailscale/Caddy; :3000 is firewalled).\n"
   "# Set GRAFANA_URL= (empty) in a session env file to opt out.\n"
   "GRAFANA_URL=\"${GRAFANA_URL-https://grafana.ts.peteches.co.uk}\"\n"
   "export GRAFANA_URL\n"
   "\n"
   "# ~/.claude.json is a bind mount inside the container, so `mv' over\n"
   "# it fails with EBUSY.  Capture jq output in a shell variable and\n"
   "# write it back with `> \"$HOME/.claude.json\"' — that opens/truncates\n"
   "# the existing file (following the bind mount to the underlying\n"
   "# session file) instead of replacing the inode.\n"
   "register_anvil_mcp() {\n"
   "  command -v jq >/dev/null 2>&1 || { log \"jq unavailable; skipping MCP self-register\"; return 0; }\n"
   "  [ -f \"$HOME/.claude.json\" ] || echo '{}' > \"$HOME/.claude.json\"\n"
   "  local new_content\n"
   "  if [ \"${CLAUDE_CONTAINER_NO_ANVIL:-0}\" = 1 ]; then\n"
   "    new_content=$(jq 'if .mcpServers then\n"
   "          .mcpServers |= (del(.anvil) | del(.[\"anvil-emacs-eval\"]))\n"
   "        else . end' \"$HOME/.claude.json\") || return 0\n"
   "    printf '%s\\n' \"$new_content\" > \"$HOME/.claude.json\"\n"
   "    return 0\n"
   "  fi\n"
   "  local bash_bin script\n"
   "  bash_bin=$(command -v bash) || return 0\n"
   "  script=$(command -v anvil-stdio.sh) || { log \"anvil-stdio.sh missing on PATH\"; return 0; }\n"
   "  new_content=$(jq \\\n"
   "    --arg cmd \"$bash_bin\" \\\n"
   "    --arg script \"$script\" \\\n"
   "    '(.mcpServers //= {})\n"
   ;; No --init-function/--stop-function: enabling and starting the
   ;; server is done once, daemon-side, by anvil_bootstrap.  A per-bridge
   ;; stop function would disable anvil for every other connected bridge
   ;; when any one of them exits.
   "     | .mcpServers.anvil = {\n"
   "         type: \"stdio\",\n"
   "         command: $cmd,\n"
   "         args: [$script, \"--server-id=anvil\"],\n"
   "         env: {}\n"
   "       }\n"
   "     | .mcpServers[\"anvil-emacs-eval\"] = {\n"
   "         type: \"stdio\",\n"
   "         command: $cmd,\n"
   "         args: [$script, \"--server-id=emacs-eval\"],\n"
   "         env: {}\n"
   "       }' \"$HOME/.claude.json\") || return 0\n"
   "  printf '%s\\n' \"$new_content\" > \"$HOME/.claude.json\"\n"
   "}\n"
   "\n"
   ;; Grafana MCP: one server covering Prometheus metrics, Loki logs and
   ;; Grafana dashboards.  Registration is gated on GRAFANA_URL so that
   ;; sessions without Grafana config do not get a server that fails on
   ;; every call; when it is unset we actively *remove* any stale entry,
   ;; keeping this idempotent in both directions.
   ;;
   ;; GRAFANA_URL comes from the per-session `env' file and
   ;; GRAFANA_SERVICE_ACCOUNT_TOKEN from the sops-encrypted
   ;; `secrets.env'; both are already exported into this shell above, and
   ;; the MCP server inherits them from claude.  The token is therefore
   ;; deliberately NOT written into ~/.claude.json — that file is rewritten
   ;; by Claude Code at runtime and is not a safe place for a credential.
   ;;
   ;; The tool allowlist is trimmed to the self-hosted stack: no OnCall,
   ;; Incident, Sift, Asserts or Grafana Cloud datasources.  --disable-write
   ;; drops the six mutating tools (create/update dashboard, datasource,
   ;; annotation) so an agent cannot rewrite the monitoring setup.
   "register_grafana_mcp() {\n"
   "  command -v jq >/dev/null 2>&1 || return 0\n"
   "  [ -f \"$HOME/.claude.json\" ] || echo '{}' > \"$HOME/.claude.json\"\n"
   "  local new_content grafana_bin\n"
   "  if [ -z \"${GRAFANA_URL:-}\" ] \\\n"
   "     || ! grafana_bin=$(command -v mcp-grafana); then\n"
   "    new_content=$(jq 'if .mcpServers then\n"
   "          .mcpServers |= del(.grafana)\n"
   "        else . end' \"$HOME/.claude.json\") || return 0\n"
   "    printf '%s\\n' \"$new_content\" > \"$HOME/.claude.json\"\n"
   "    return 0\n"
   "  fi\n"
   "  if [ -z \"${GRAFANA_SERVICE_ACCOUNT_TOKEN:-}\" ]; then\n"
   "    log \"GRAFANA_URL set but GRAFANA_SERVICE_ACCOUNT_TOKEN is not;\"\n"
   "    log \"  grafana MCP will register but calls will be unauthorised\"\n"
   "  fi\n"
   "  new_content=$(jq \\\n"
   "    --arg cmd \"$grafana_bin\" \\\n"
   "    '(.mcpServers //= {})\n"
   "     | .mcpServers.grafana = {\n"
   "         type: \"stdio\",\n"
   "         command: $cmd,\n"
   "         args: [\n"
   "           \"-t\", \"stdio\",\n"
   ;; Must stay on one physical line: jq string literals have no
   ;; line-continuation escape.
   "           \"--enabled-tools=search,datasource,prometheus,loki,dashboard,folder,navigation,annotations\",\n"
   "           \"--disable-write\",\n"
   "           \"--disable-proxied\"\n"
   "         ],\n"
   "         env: {}\n"
   "       }' \"$HOME/.claude.json\") || return 0\n"
   "  printf '%s\\n' \"$new_content\" > \"$HOME/.claude.json\"\n"
   "}\n"
   "\n"
   ;; The comfyui entry arrives from the host ~/.claude.json sync, where
   ;; `guix home' registered it as an absolute store path built from the
   ;; *host's* channel pin.  The container builds its own comfyui-mcp from
   ;; the pin in this file, so the two store paths differ even at identical
   ;; versions.  With --nesting the whole host store is mapped read-only and
   ;; the host path happens to resolve; without it the container sees only
   ;; its own manifest closure, the host path is absent, and the server
   ;; merely reports "failed to connect".
   ;;
   ;; So rewrite the entry to the container profile's binary, exactly as
   ;; register_anvil_mcp and register_grafana_mcp do for theirs.  If
   ;; comfyui-mcp is not on PATH, drop the entry rather than leave a
   ;; dangling one.
   "register_comfyui_mcp() {\n"
   "  command -v jq >/dev/null 2>&1 || return 0\n"
   "  [ -f \"$HOME/.claude.json\" ] || echo '{}' > \"$HOME/.claude.json\"\n"
   "  local new_content comfyui_bin\n"
   "  if ! comfyui_bin=$(command -v comfyui-mcp); then\n"
   "    new_content=$(jq 'if .mcpServers then\n"
   "          .mcpServers |= del(.comfyui)\n"
   "        else . end' \"$HOME/.claude.json\") || return 0\n"
   "    printf '%s\\n' \"$new_content\" > \"$HOME/.claude.json\"\n"
   "    return 0\n"
   "  fi\n"
   "  new_content=$(jq \\\n"
   "    --arg cmd \"$comfyui_bin\" \\\n"
   "    '(.mcpServers //= {})\n"
   "     | .mcpServers.comfyui = {\n"
   "         type: \"stdio\",\n"
   "         command: $cmd,\n"
   "         args: [],\n"
   "         env: {}\n"
   "       }' \"$HOME/.claude.json\") || return 0\n"
   "  printf '%s\\n' \"$new_content\" > \"$HOME/.claude.json\"\n"
   "}\n"
   "\n"
   ;; anvil.el >= 1.3 splits module loading (anvil-enable) from request
   ;; handling (anvil-server-start); bridges error with \"No active MCP
   ;; server\" unless the server is started.  Run both idempotently so
   ;; sessions whose seeded init.el predates the split still work.
   "anvil_bootstrap() {\n"
   ;; anvil-server-start lives in anvil-server-commands.el, which nothing
   ;; on the (require (quote anvil)) path loads — require it explicitly.
   "  emacsclient -e '(progn (unless (bound-and-true-p anvil--enabled) (anvil-enable)) (require (quote anvil-server-commands) nil t) (when (and (fboundp (quote anvil-server-start)) (not (bound-and-true-p anvil-server--running))) (anvil-server-start)) t)' >/dev/null 2>&1 \\\n"
   "    || log \"anvil server bootstrap failed\"\n"
   "}\n"
   "\n"
   "start_anvil_daemon() {\n"
   "  [ \"${CLAUDE_CONTAINER_NO_ANVIL:-0}\" = 1 ] && { log \"anvil disabled (--no-anvil)\"; return 0; }\n"
   "  command -v emacs >/dev/null 2>&1 || return 0\n"
   "  emacsclient -e t >/dev/null 2>&1 && { anvil_bootstrap; return 0; }\n"
   "  local log_file=\"$HOME/.config/emacs/daemon.log\"\n"
   "  mkdir -p \"$(dirname \"$log_file\")\"\n"
   "  if ! emacs --daemon >>\"$log_file\" 2>&1; then\n"
   "    log \"emacs --daemon failed; see $log_file\"\n"
   "    return 0\n"
   "  fi\n"
   "  local waited=0\n"
   "  while [ \"$waited\" -lt 60 ]; do\n"
   "    if emacsclient -e '(fboundp (quote anvil-enable))' 2>/dev/null \\\n"
   "         | grep -q 't$'; then\n"
   "      anvil_bootstrap\n"
   "      return 0\n"
   "    fi\n"
   "    sleep 1\n"
   "    waited=$((waited + 1))\n"
   "  done\n"
   "  log \"anvil-enable not defined after 60s; see $log_file\"\n"
   "}\n"
   "\n"
   "register_anvil_mcp\n"
   "register_grafana_mcp\n"
   "register_comfyui_mcp\n"
   "start_anvil_daemon\n"
   "\n"
   "# Per-session init hook, sourced after anvil is up.\n"
   "if [ -f \"$HOME/.claude-session/init.sh\" ]; then\n"
   "  # shellcheck disable=SC1091\n"
   "  . \"$HOME/.claude-session/init.sh\"\n"
   "fi\n"
   "\n"
   "if [ \"${CLAUDE_CONTAINER_SHELL:-0}\" = 1 ]; then\n"
   "  exec bash -i\n"
   "fi\n"
   "\n"
   "exec claude \"$@\"\n"))

;; Wrapper script.  bash-minimal supplies the shebang; the manifest and
;; anvil-init paths are substituted from the store.  `guix' itself is
;; resolved from the outer PATH at invocation time.
(define claude-container-wrapper
  (mixed-text-file
   "claude-container-wrapper.sh"
   "#!" bash-minimal "/bin/bash\n"
   "set -eu\n"
   "set -o pipefail\n"
   "\n"
   ": \"${CLAUDE_CONTAINER_SESSIONS_DIR:=$HOME/.claude-sessions}\"\n"
   "\n"
   "session=default\n"
   "migrate=no\n"
   "# --worktree is the default; --no-worktree opts out.  When $PWD is\n"
   "# not a git working tree, we silently fall back to no-worktree unless\n"
   "# the user asked for it explicitly.\n"
   "worktree=yes\n"
   "worktree_explicit=no\n"
   "worktree_branch=\"\"\n"
   "keep_worktree=no\n"
   "no_anvil=no\n"
   "shell_mode=no\n"
   "ephemeral=no\n"
   "secrets_shm=\"\"\n"
   "# Guix nesting: off by default so ordinary sessions stay isolated.\n"
   "# 'unset' means \"not specified on the command line\", which lets a\n"
   "# per-session marker file decide instead.\n"
   "nesting=unset\n"
   "extra_opts=()\n"
   "claude_args=()\n"
   "\n"
   "usage() {\n"
   "  cat <<'EOF'\n"
   "Usage: claude [--session NAME] [--migrate y|n] [--share PATH[=DEST]]\n"
   "              [--expose PATH[=DEST]] [--worktree [BRANCH]] [--no-worktree]\n"
   "              [--keep-worktree] [--no-anvil] [--nesting|--no-nesting]\n"
   "              [--shell] [--rm|--ephemeral]\n"
   "              [--list-sessions] [-h|--help] [-- CLAUDE_ARG ...]\n"
   "\n"
   "Runs claude-code inside 'guix shell --container'.\n"
   "\n"
   "  --session NAME     Use session NAME (default: 'default'). Session dirs\n"
   "                     live under $CLAUDE_CONTAINER_SESSIONS_DIR (default:\n"
   "                     ~/.claude-sessions). Each session persists:\n"
   "                       .claude/         -> ~/.claude\n"
   "                       .claude.json     -> ~/.claude.json\n"
   "                       .config/emacs/   -> ~/.config/emacs (daemon.log lives here)\n"
   "                       .cache/          -> ~/.cache        (XDG_CACHE_HOME)\n"
   "                       .local/          -> ~/.local        (XDG_DATA/STATE_HOME)\n"
   "                       .npm/            -> ~/.npm          (npx cache)\n"
   "                       .bash_history    -> ~/.bash_history\n"
   "                       init.sh          -> sourced after anvil starts\n"
   "                       env              -> auto-exported KEY=VALUE lines\n"
   "                       secrets.env      -> sops-encrypted dotenv,\n"
   "                                           decrypted on the host and\n"
   "                                           exported into the container;\n"
   "                                           overrides ~/.claude/secrets.env\n"
   "                       shared-dirs      -> extra --share bind-mounts\n"
   "                       packages         -> extra package specs, one per\n"
   "                                           line, added to the container\n"
   "                       manifest.scm     -> extra manifest, merged with\n"
   "                                           the base container manifest\n"
   "                       claude-overrides.json\n"
   "                                        -> jq-merged over host\n"
   "                                           ~/.claude/settings.json every\n"
   "                                           launch (session wins)\n"
   "                     First use seeds .claude/ and .claude.json from the\n"
   "                     host's ~/.claude and ~/.claude.json when available.\n"
   "                     On every launch, host ~/.claude/settings.json is\n"
   "                     merged with claude-overrides.json (if present) and\n"
   "                     written to <session>/.claude/settings.json so global\n"
   "                     mcpServers / hook changes propagate automatically.\n"
   "  --migrate y|n      Migrate legacy session layouts (pre-2026-07). Default: n.\n"
   "  --share PATH       Extra bind-mount (read-write). Repeatable.\n"
   "  --expose PATH      Extra bind-mount (read-only). Repeatable.\n"
   "  --worktree [BR]    Provision a git worktree of $PWD and use it as CWD.\n"
   "                     This is the DEFAULT. Optional BRANCH names a branch\n"
   "                     to check out or create. Removed on exit unless\n"
   "                     --keep-worktree. If $PWD is not a git working tree\n"
   "                     the wrapper silently falls back to no-worktree\n"
   "                     (only errors when --worktree was passed explicitly).\n"
   "  --no-worktree      Disable the default worktree behavior and run\n"
   "                     directly in $PWD (or shared-dirs[0]).\n"
   "  --keep-worktree    Do not remove the worktree on exit.\n"
   "  --no-anvil         Skip the emacs daemon / anvil MCP entirely.\n"
   "  --nesting, --guix  Make Guix usable inside the container: passes\n"
   "                     'guix shell -W', which maps the full /gnu/store\n"
   "                     (read-only), the build daemon socket and guix's\n"
   "                     caches, and exposes the host's pulled guix\n"
   "                     (~/.config/guix/current) so CHANNEL modules\n"
   "                     ((nongnu ...), (sops ...), ...) resolve.  Without\n"
   "                     it the container has no /var/guix and only the\n"
   "                     manifest closure, so 'guix build' cannot run at\n"
   "                     all.  Also makes store-symlinked host config\n"
   "                     resolve, notably ~/.config/git/config.\n"
   "                     OFF by default: a reachable daemon can build and\n"
   "                     install arbitrary derivations on the host.  A\n"
   "                     session opts in permanently by creating an empty\n"
   "                     'nesting' file in its session dir.\n"
   "  --no-nesting       Force nesting off, overriding the marker file.\n"
   "  --shell            Drop into container bash instead of launching claude.\n"
   "  --rm, --ephemeral  Use a throwaway session (mktemp) removed on exit.\n"
   "  --list-sessions    Print existing session directory names and exit.\n"
   "  -h, --help         Show this help and exit.\n"
   "\n"
   "Each session may contain a 'shared-dirs' file (one absolute path per\n"
   "line, blank lines ignored). If present without --worktree, line 1\n"
   "becomes the container's working directory; every line is added as\n"
   "a --share bind-mount.\n"
   "\n"
   "Extra tooling can be layered onto the base container manifest per\n"
   "session, without editing it:\n"
   "\n"
   "  packages       Package specs added to the container, whitespace- or\n"
   "                 newline-separated. '#' starts a comment. Example, for\n"
   "                 a Go project:\n"
   "                     go\n"
   "                     gopls\n"
   "                     delve\n"
   "  manifest.scm   A manifest file passed as an additional --manifest.\n"
   "                 Use this when specs are not enough: package\n"
   "                 transformations, custom package definitions, or a\n"
   "                 specific package variant.\n"
   "\n"
   "Both are additive: the base manifest (claude-code, anvil, shared\n"
   "tooling) is always present.\n"
   "\n"
   "Secrets are handled separately from the plaintext 'env' file, via\n"
   "sops-encrypted dotenv files (safe to commit and to symlink from the\n"
   "store, since they are ciphertext). Two are read, in order:\n"
   "\n"
   "  ~/.claude/secrets.env        shared by every session\n"
   "  <session-dir>/secrets.env    that session only\n"
   "\n"
   "Both are sourced under 'set -a', shared first, so a session key of\n"
   "the same name overrides the shared one. Put homelab-wide credentials\n"
   "(e.g. GRAFANA_SERVICE_ACCOUNT_TOKEN) in the shared file so rotating\n"
   "one is a single-file edit.\n"
   "\n"
   "The wrapper runs 'sops -d' on the HOST, where gpg-agent lives, into\n"
   "a private /dev/shm file that is exposed read-only into the container\n"
   "and shredded on exit. Plaintext never touches the session dir, the\n"
   "store, or the guix command line. Requires 'sops' on the host PATH.\n"
   "\n"
   "Remaining args (or args after --) are forwarded to claude inside the\n"
   "container.\n"
   "EOF\n"
   "}\n"
   "\n"
   "while [ $# -gt 0 ]; do\n"
   "  case \"$1\" in\n"
   "    --session)         session=\"$2\"; shift 2 ;;\n"
   "    --session=*)       session=\"${1#*=}\"; shift ;;\n"
   "    --migrate)         migrate=\"$2\"; shift 2 ;;\n"
   "    --migrate=*)       migrate=\"${1#*=}\"; shift ;;\n"
   "    --share)           extra_opts+=(\"--share=$2\"); shift 2 ;;\n"
   "    --share=*)         extra_opts+=(\"--share=${1#*=}\"); shift ;;\n"
   "    --expose)          extra_opts+=(\"--expose=$2\"); shift 2 ;;\n"
   "    --expose=*)        extra_opts+=(\"--expose=${1#*=}\"); shift ;;\n"
   "    --worktree)\n"
   "      worktree=yes; worktree_explicit=yes\n"
   "      if [ $# -ge 2 ] && [ -n \"${2:-}\" ] && [ \"${2#-}\" = \"$2\" ]; then\n"
   "        worktree_branch=\"$2\"; shift 2\n"
   "      else\n"
   "        shift\n"
   "      fi ;;\n"
   "    --worktree=*)\n"
   "      worktree=yes; worktree_explicit=yes\n"
   "      worktree_branch=\"${1#*=}\"; shift ;;\n"
   "    --no-worktree)     worktree=no; shift ;;\n"
   "    --keep-worktree)   keep_worktree=yes; shift ;;\n"
   "    --no-anvil)        no_anvil=yes; shift ;;\n"
   "    --nesting|--guix)  nesting=yes; shift ;;\n"
   "    --no-nesting)      nesting=no; shift ;;\n"
   "    --shell)           shell_mode=yes; shift ;;\n"
   "    --rm|--ephemeral)  ephemeral=yes; shift ;;\n"
   "    --list-sessions)\n"
   "      if [ -d \"$CLAUDE_CONTAINER_SESSIONS_DIR\" ]; then\n"
   "        find \"$CLAUDE_CONTAINER_SESSIONS_DIR\" -mindepth 1 -maxdepth 1 \\\n"
   "             -type d -printf '%f\\n' | sort -u\n"
   "      fi\n"
   "      exit 0 ;;\n"
   "    -h|--help)         usage; exit 0 ;;\n"
   "    --)                shift; claude_args+=(\"$@\"); break ;;\n"
   "    *)                 claude_args+=(\"$1\"); shift ;;\n"
   "  esac\n"
   "done\n"
   "\n"
   "# Ephemeral session: mktemp dir cleaned on exit.\n"
   "if [ \"$ephemeral\" = yes ]; then\n"
   "  mkdir -p \"$CLAUDE_CONTAINER_SESSIONS_DIR\"\n"
   "  session=$(basename \"$(mktemp -du -p \"$CLAUDE_CONTAINER_SESSIONS_DIR\" ephemeral-XXXXXX)\")\n"
   "fi\n"
   "\n"
   "session_dir=\"${CLAUDE_CONTAINER_SESSIONS_DIR}/${session}\"\n"
   "mkdir -p \"$session_dir\"\n"
   "\n"
   "# Detect the pre-2026-07 layout, where the session_dir itself was\n"
   "# bind-mounted onto ~/.claude and all state lived at its top level.\n"
   "legacy=no\n"
   "if [ ! -d \"$session_dir/.claude\" ]; then\n"
   "  # NOTE: every file a session may legitimately contain must be listed\n"
   "  # here, or a fresh session holding only that file is misdetected as\n"
   "  # legacy and the wrapper aborts.  Adding a new session-dir file means\n"
   "  # adding a `! -name' clause below.\n"
   "  if [ -n \"$(find \"$session_dir\" -mindepth 1 -maxdepth 1 \\\n"
   "                  ! -name .claude ! -name .claude.json \\\n"
   "                  ! -name shared-dirs ! -name init.sh ! -name env \\\n"
   "                  ! -name .cache ! -name .local ! -name .npm \\\n"
   "                  ! -name .bash_history ! -name worktrees \\\n"
   "                  ! -name nesting ! -name claude-overrides.json \\\n"
   "                  ! -name .config -print -quit 2>/dev/null)\" ]; then\n"
   "    legacy=yes\n"
   "  fi\n"
   "fi\n"
   "\n"
   "if [ \"$legacy\" = yes ]; then\n"
   "  if [ \"$migrate\" = y ] || [ \"$migrate\" = yes ]; then\n"
   "    tmp_claude=\"$session_dir/.claude.in-progress\"\n"
   "    rm -rf \"$tmp_claude\"\n"
   "    mkdir \"$tmp_claude\"\n"
   "    find \"$session_dir\" -mindepth 1 -maxdepth 1 \\\n"
   "         ! -name .claude ! -name .claude.json ! -name shared-dirs \\\n"
   "         ! -name .claude.in-progress \\\n"
   "         -exec mv {} \"$tmp_claude/\" \\;\n"
   "    mv \"$tmp_claude\" \"$session_dir/.claude\"\n"
   "    echo \"Migrated legacy session '$session' -> $session_dir/.claude/\" >&2\n"
   "  else\n"
   "    echo \"Session '$session' at $session_dir uses the legacy layout.\" >&2\n"
   "    echo \"Re-run with --migrate y to move its contents into\" >&2\n"
   "    echo \"$session_dir/.claude/.\" >&2\n"
   "    exit 1\n"
   "  fi\n"
   "fi\n"
   "\n"
   "# Seed .claude/ from the host on first use.\n"
   "if [ ! -d \"$session_dir/.claude\" ]; then\n"
   "  if [ -d \"$HOME/.claude\" ]; then\n"
   "    cp -a \"$HOME/.claude\" \"$session_dir/.claude\"\n"
   "  else\n"
   "    mkdir -p \"$session_dir/.claude\"\n"
   "  fi\n"
   "fi\n"
   "\n"
   "# Seed .claude.json from the host on first use.\n"
   "if [ ! -f \"$session_dir/.claude.json\" ]; then\n"
   "  if [ -f \"$HOME/.claude.json\" ]; then\n"
   "    cp -a \"$HOME/.claude.json\" \"$session_dir/.claude.json\"\n"
   "  else\n"
   "    printf '{}' > \"$session_dir/.claude.json\"\n"
   "  fi\n"
   "fi\n"
   "\n"
   "# On every launch, sync mcpServers from host ~/.claude.json into the\n"
   "# session's copy so newly-registered MCP servers (like comfyui via\n"
   "# guix home reconfigure) reach existing sessions.  Other session\n"
   "# state (project history, per-project settings) is preserved.\n"
   ;; Per-session servers come from claude-overrides.json's mcpServers
   ;; and must land in .claude.json (user scope): Claude Code ignores
   ;; mcpServers in settings.json, so the settings merge below is not
   ;; enough.  Session entries win over host ones on name clashes.
   "if [ -f \"$HOME/.claude.json\" ] && command -v jq >/dev/null 2>&1; then\n"
   "  session_claude_json=\"$session_dir/.claude.json\"\n"
   "  session_overrides=\"$session_dir/claude-overrides.json\"\n"
   "  if [ -f \"$session_overrides\" ]; then\n"
   "    new_content=$(jq -s '.[0] * {mcpServers: ((.[1].mcpServers // {}) * (.[2].mcpServers // {}))}' \\\n"
   "                      \"$session_claude_json\" \"$HOME/.claude.json\" \\\n"
   "                      \"$session_overrides\") \\\n"
   "      && printf '%s\\n' \"$new_content\" > \"$session_claude_json\"\n"
   "  else\n"
   "    new_content=$(jq -s '.[0] * {mcpServers: (.[1].mcpServers // {})}' \\\n"
   "                      \"$session_claude_json\" \"$HOME/.claude.json\") \\\n"
   "      && printf '%s\\n' \"$new_content\" > \"$session_claude_json\"\n"
   "  fi\n"
   "fi\n"
   "\n"
   "# Merge host ~/.claude/settings.json with per-session overrides on\n"
   "# every launch, so host mcpServers/hook edits propagate without\n"
   "# manual session updates.  Session overrides (opt-in) live at\n"
   "# $session_dir/claude-overrides.json and are recursively merged\n"
   "# over the host base with jq's `*' operator (session wins on\n"
   "# scalars/arrays; objects merge recursively).\n"
   "host_settings=\"$HOME/.claude/settings.json\"\n"
   "session_overrides=\"$session_dir/claude-overrides.json\"\n"
   "merged_settings=\"$session_dir/.claude/settings.json\"\n"
   "if [ -f \"$host_settings\" ]; then\n"
   "  mkdir -p \"$(dirname \"$merged_settings\")\"\n"
   "  if [ -f \"$session_overrides\" ]; then\n"
   "    if ! jq -s '.[0] * .[1]' \"$host_settings\" \"$session_overrides\" \\\n"
   "         > \"$merged_settings.new\"; then\n"
   "      echo \"claude-container: failed to merge overrides from\" >&2\n"
   "      echo \"  $session_overrides\" >&2\n"
   "      rm -f \"$merged_settings.new\"\n"
   "      exit 1\n"
   "    fi\n"
   "  else\n"
   "    cp \"$host_settings\" \"$merged_settings.new\"\n"
   "  fi\n"
   "  mv \"$merged_settings.new\" \"$merged_settings\"\n"
   "fi\n"
   "\n"
   "# Persistent session sub-dirs (created if missing, no-op otherwise).\n"
   "mkdir -p \"$session_dir/.config/emacs\" \\\n"
   "         \"$session_dir/.cache\" \\\n"
   "         \"$session_dir/.local/share\" \\\n"
   "         \"$session_dir/.local/state\" \\\n"
   "         \"$session_dir/.npm\"\n"
   "\n"
   "# Seed the anvil init.el on first launch.\n"
   "if [ ! -f \"$session_dir/.config/emacs/init.el\" ]; then\n"
   "  install -m 0644 " claude-container-anvil-init " \\\n"
   "          \"$session_dir/.config/emacs/init.el\"\n"
   "fi\n"
   "\n"
   "# bash_history file must exist for the bind-mount to succeed.\n"
   "touch \"$session_dir/.bash_history\"\n"
   "\n"
   "# --- guix nesting resolution ---\n"
   "# A session opts in permanently by creating a 'nesting' marker file in\n"
   "# its session dir; --nesting/--no-nesting override per invocation.\n"
   "if [ \"$nesting\" = unset ]; then\n"
   "  if [ -e \"$session_dir/nesting\" ]; then nesting=yes; else nesting=no; fi\n"
   "fi\n"
   "\n"
   "# --- worktree provisioning ---\n"
   "# Default is worktree=yes; if $PWD isn't a git working tree we fall\n"
   "# back to no-worktree, unless the user asked for it explicitly with\n"
   "# --worktree, in which case we error out.\n"
   "worktree_path=\"\"\n"
   "repo_root=\"\"\n"
   "if [ \"$worktree\" = yes ]; then\n"
   "  if ! git -C \"$PWD\" rev-parse --show-toplevel >/dev/null 2>&1; then\n"
   "    if [ \"$worktree_explicit\" = yes ]; then\n"
   "      echo \"--worktree requires \\$PWD to be inside a git working tree.\" >&2\n"
   "      exit 1\n"
   "    fi\n"
   "    worktree=no\n"
   "  fi\n"
   "fi\n"
   "if [ \"$worktree\" = yes ]; then\n"
   "  repo_root=$(git -C \"$PWD\" rev-parse --show-toplevel)\n"
   "  repo_name=$(basename \"$repo_root\")\n"
   "  mkdir -p \"$session_dir/worktrees\"\n"
   "  worktree_path=$(mktemp -du -p \"$session_dir/worktrees\" \"${repo_name}-XXXXXX\")\n"
   "  if [ -n \"$worktree_branch\" ]; then\n"
   "    if git -C \"$repo_root\" show-ref --verify --quiet \"refs/heads/$worktree_branch\"; then\n"
   "      git -C \"$repo_root\" worktree add \"$worktree_path\" \"$worktree_branch\"\n"
   "    else\n"
   "      git -C \"$repo_root\" worktree add -b \"$worktree_branch\" \"$worktree_path\"\n"
   "    fi\n"
   "  else\n"
   "    git -C \"$repo_root\" worktree add --detach \"$worktree_path\"\n"
   "  fi\n"
   "  echo \"Worktree: $worktree_path\" >&2\n"
   "fi\n"
   "\n"
   "# --- shared-dirs / primary_dir resolution ---\n"
   "share_paths=()\n"
   "if [ -f \"$session_dir/shared-dirs\" ]; then\n"
   "  while IFS= read -r line || [ -n \"$line\" ]; do\n"
   "    [ -z \"$line\" ] && continue\n"
   "    share_paths+=(\"$line\")\n"
   "  done < \"$session_dir/shared-dirs\"\n"
   "fi\n"
   "\n"
   ;; Per-session manifest extensions.  `guix shell' merges every -m it
   ;; is given and accepts bare specs positionally, so both files are
   ;; purely additive on top of the base container manifest.
   "# --- per-session packages / manifest ---\n"
   "session_pkgs=()\n"
   "if [ -f \"$session_dir/packages\" ]; then\n"
   "  # Unquoted $line word-splits, so specs may be newline- or\n"
   "  # whitespace-separated; `set -f' keeps a spec like `python*' from\n"
   "  # globbing against the CWD.\n"
   "  set -f\n"
   "  while IFS= read -r line || [ -n \"$line\" ]; do\n"
   "    line=\"${line%%#*}\"\n"
   "    # shellcheck disable=SC2206\n"
   "    for spec in $line; do\n"
   "      session_pkgs+=(\"$spec\")\n"
   "    done\n"
   "  done < \"$session_dir/packages\"\n"
   "  set +f\n"
   "fi\n"
   "\n"
   "if [ -f \"$session_dir/manifest.scm\" ]; then\n"
   "  extra_opts+=(\"--manifest=$session_dir/manifest.scm\")\n"
   "fi\n"
   "\n"
   "if [ -n \"$worktree_path\" ]; then\n"
   "  primary_dir=\"$worktree_path\"\n"
   "  share_opts=(\"--share=$worktree_path\")\n"
   "  for p in \"${share_paths[@]}\"; do\n"
   "    share_opts+=(\"--share=$p\")\n"
   "  done\n"
   "elif [ ${#share_paths[@]} -gt 0 ]; then\n"
   "  primary_dir=\"${share_paths[0]}\"\n"
   "  share_opts=()\n"
   "  for p in \"${share_paths[@]}\"; do\n"
   "    share_opts+=(\"--share=$p\")\n"
   "  done\n"
   "else\n"
   "  primary_dir=\"$PWD\"\n"
   "  share_opts=(\"--share=$PWD\")\n"
   "fi\n"
   "\n"
   "if [ ! -d \"$primary_dir\" ]; then\n"
   "  echo \"Primary directory '$primary_dir' does not exist.\" >&2\n"
   "  exit 1\n"
   "fi\n"
   "\n"
   ;; --- per-session sops secrets --------------------------------------
   ;; A session may ship `secrets.env': a sops-encrypted dotenv file,
   ;; committed to the repo and symlinked into the session dir from the
   ;; store (safe — it is ciphertext).  We decrypt it HERE, on the host,
   ;; where gpg-agent lives, into a private tmpfs file under /dev/shm.
   ;; That file is exposed read-only into the container and shredded on
   ;; exit (see cleanup()).  Plaintext never touches the session dir,
   ;; the store, or the `guix' command line, so it stays off disk and
   ;; out of `ps'.  This is deliberately a separate channel from the
   ;; plaintext `env' file.  Placed after the last early `exit' so a
   ;; failure path can never orphan the decrypted tmpfs file: from here
   ;; to the cleanup trap there are no exits.
   ;; Two tiers, concatenated into one tmpfs file in this order:
   ;;
   ;;   1. ~/.claude/secrets.env  — shared by every session (symlinked
   ;;      from configs/claude/defaults).  For credentials that are a
   ;;      property of the homelab rather than of one project, such as
   ;;      GRAFANA_SERVICE_ACCOUNT_TOKEN.  Encrypting it once here beats
   ;;      copying the same ciphertext into every session dir, which
   ;;      would make rotation an N-file edit.
   ;;   2. $session_dir/secrets.env — per-session, as before.
   ;;
   ;; Order matters: the file is sourced sequentially under `set -a', so
   ;; a session key of the same name overrides the shared one.
   "# --- shared + per-session sops secrets ---\n"
   "shared_secrets=\"$HOME/.claude/secrets.env\"\n"
   "if [ -f \"$shared_secrets\" ] || [ -f \"$session_dir/secrets.env\" ]; then\n"
   "  if command -v sops >/dev/null 2>&1; then\n"
   "    secrets_shm=$(mktemp \"/dev/shm/claude-secrets-${session}-XXXXXX\") || {\n"
   "      echo \"claude-container: failed to allocate tmpfs for secrets\" >&2\n"
   "      exit 1\n"
   "    }\n"
   "    chmod 600 \"$secrets_shm\"\n"
   "    for secrets_file in \"$shared_secrets\" \"$session_dir/secrets.env\"; do\n"
   "      [ -f \"$secrets_file\" ] || continue\n"
   "      if ! sops -d --output-type dotenv \"$secrets_file\" \\\n"
   "           >> \"$secrets_shm\"; then\n"
   "        echo \"claude-container: sops failed to decrypt\" >&2\n"
   "        echo \"  $secrets_file\" >&2\n"
   "        shred -u \"$secrets_shm\" 2>/dev/null || rm -f \"$secrets_shm\"\n"
   "        exit 1\n"
   "      fi\n"
   ;; Guard against a decrypted file with no trailing newline running
   ;; its last KEY=VALUE into the next file's first one.
   "      printf '\\n' >> \"$secrets_shm\"\n"
   "    done\n"
   "  else\n"
   "    echo \"claude-container: a secrets.env is present but sops is not\" >&2\n"
   "    echo \"  on PATH; secrets will not be available\" >&2\n"
   "  fi\n"
   "fi\n"
   "\n"
   "# --- container env ---\n"
   "container_env=(\n"
   "  \"GUIX_LOCPATH=" glibc-locales "/lib/locale\"\n"
   "  \"XDG_CACHE_HOME=$HOME/.cache\"\n"
   "  \"XDG_DATA_HOME=$HOME/.local/share\"\n"
   "  \"XDG_STATE_HOME=$HOME/.local/state\"\n"
   "  \"XDG_CONFIG_HOME=$HOME/.config\"\n"
   "  \"CLAUDE_SESSION=$session\"\n"
   ")\n"
   "\n"
   "if [ \"$no_anvil\" = yes ]; then\n"
   "  container_env+=(\"CLAUDE_CONTAINER_NO_ANVIL=1\")\n"
   "fi\n"
   "if [ \"$shell_mode\" = yes ]; then\n"
   "  container_env+=(\"CLAUDE_CONTAINER_SHELL=1\")\n"
   "fi\n"
   "\n"
   "opts=(\n"
   "  --container\n"
   "  --network\n"
   "  --preserve=^TERM$\n"
   "  --preserve=^COLORTERM$\n"
   "  --preserve=^LANG$\n"
   "  --preserve=^LC_\n"
   "  --preserve=^ANTHROPIC_\n"
   "  --preserve=^CLAUDE_\n"
   "  --preserve=^GH_TOKEN$\n"
   "  --preserve=^GITHUB_TOKEN$\n"
   "  --preserve=^OPENAI_\n"
   "  --preserve=^GEMINI_\n"
   "  --preserve=^GOOGLE_\n"
   "  --preserve=^AWS_\n"
   "  --preserve=^GRAFANA_\n"
   "  --preserve=^NPM_TOKEN$\n"
   "  --preserve=^NODE_AUTH_TOKEN$\n"
   "  --preserve=^CARGO_\n"
   "  --preserve=^GOPATH$\n"
   "  --preserve=^GOMODCACHE$\n"
   "  --preserve=^COMFYUI_\n"
   "  --preserve=^NUG_\n"
   "  --preserve=^EDITOR$\n"
   "  --preserve=^VISUAL$\n"
   "  --preserve=^PAGER$\n"
   "  --preserve=^NO_COLOR$\n"
   "  --preserve=^FORCE_COLOR$\n"
   "  --preserve=^SSH_AUTH_SOCK$\n"
   "  \"--share=$session_dir/.claude=$HOME/.claude\"\n"
   "  \"--share=$session_dir/.claude.json=$HOME/.claude.json\"\n"
   "  \"--share=$session_dir/.config/emacs=$HOME/.config/emacs\"\n"
   "  \"--share=$session_dir/.cache=$HOME/.cache\"\n"
   "  \"--share=$session_dir/.local=$HOME/.local\"\n"
   "  \"--share=$session_dir/.npm=$HOME/.npm\"\n"
   "  \"--share=$session_dir/.bash_history=$HOME/.bash_history\"\n"
   "  \"--share=$session_dir=$HOME/.claude-session\"\n"
   "  \"${share_opts[@]}\"\n"
   ")\n"
   "\n"
   "[ -e \"$HOME/.gitconfig\" ]     && opts+=(--expose=\"$HOME/.gitconfig\")\n"
   "[ -e \"$HOME/.config/git\" ]    && opts+=(--expose=\"$HOME/.config/git\")\n"
   "[ -e \"$HOME/.ssh\" ]           && opts+=(--expose=\"$HOME/.ssh\")\n"
   "\n"
   "# --- guix nesting ---\n"
   "# `guix shell -W' (--nesting) maps the things Guix itself needs inside a\n"
   "# container: the FULL /gnu/store (read-only), the daemon socket, and\n"
   "# guix's cache/profile dirs.  Without it the container sees only the\n"
   "# manifest closure (~200 store items) and has no /var/guix at all, so\n"
   "# `guix build' cannot even compute a derivation -- it needs a store\n"
   "# connection.  Missing sources are skipped, so -W degrades gracefully.\n"
   "#\n"
   "# Exposing the pulled guix additionally brings the CHANNEL modules\n"
   "# (nonguix, sops-guix, guix-science, peteches) into scope; the guix in\n"
   "# the container manifest is plain upstream and cannot resolve them, so\n"
   "# `guix system build' on a config importing (nongnu ...) or (sops ...)\n"
   "# fails without this.\n"
   "#\n"
   "# Side effect worth knowing: -W maps the whole store, which also makes\n"
   "# store-symlinked host config resolve -- notably ~/.config/git/config,\n"
   "# which otherwise dangles (no user.name/email, no commit.gpgSign).\n"
   "#\n"
   "# Off by default: -W widens the sandbox a long way (a reachable daemon\n"
   "# can build and install arbitrary derivations on the host).  Opt in per\n"
   "# session with a 'nesting' marker file, or per run with --nesting.\n"
   "if [ \"$nesting\" = yes ]; then\n"
   "  opts+=(-W)\n"
   "  if [ -e \"$HOME/.config/guix/current\" ]; then\n"
   "    opts+=(--expose=\"$HOME/.config/guix/current\")\n"
   "    container_env+=(\"CLAUDE_CONTAINER_GUIX_CURRENT=$HOME/.config/guix/current\")\n"
   "  fi\n"
   "fi\n"
   "if [ -n \"${SSH_AUTH_SOCK:-}\" ] && [ -e \"$SSH_AUTH_SOCK\" ]; then\n"
   "  opts+=(--expose=\"$SSH_AUTH_SOCK\")\n"
   "fi\n"
   ;; Decrypted secrets tmpfs, read-only, at a fixed in-container path
   ;; the startup script sources.  The host path (a random /dev/shm
   ;; name) is remapped to a stable name so the startup script needn't
   ;; know it.
   "if [ -n \"$secrets_shm\" ]; then\n"
   "  opts+=(--expose=\"$secrets_shm=$HOME/.claude-session-secrets\")\n"
   "fi\n"
   "\n"
   "cd \"$primary_dir\"\n"
   "\n"
   "cleanup() {\n"
   "  local rc=$?\n"
   "  if [ -n \"$worktree_path\" ] && [ \"$keep_worktree\" != yes ]; then\n"
   "    if [ -n \"$repo_root\" ]; then\n"
   "      git -C \"$repo_root\" worktree remove --force \"$worktree_path\" 2>/dev/null \\\n"
   "        || rm -rf \"$worktree_path\"\n"
   "    else\n"
   "      rm -rf \"$worktree_path\"\n"
   "    fi\n"
   "  fi\n"
   "  if [ \"$ephemeral\" = yes ]; then\n"
   "    rm -rf \"$session_dir\"\n"
   "  fi\n"
   "  if [ -n \"$secrets_shm\" ]; then\n"
   "    shred -u \"$secrets_shm\" 2>/dev/null || rm -f \"$secrets_shm\"\n"
   "  fi\n"
   "  exit \"$rc\"\n"
   "}\n"
   "\n"
   ;; Secrets always force the wait-and-cleanup path: on the exec path
   ;; the wrapper is replaced and the trap never fires, which would
   ;; leave the decrypted tmpfs file behind for the rest of the boot.
   "needs_cleanup=no\n"
   "if [ \"$ephemeral\" = yes ] \\\n"
   "   || [ -n \"$secrets_shm\" ] \\\n"
   "   || { [ -n \"$worktree_path\" ] && [ \"$keep_worktree\" != yes ]; }; then\n"
   "  needs_cleanup=yes\n"
   "  trap cleanup EXIT INT TERM\n"
   "fi\n"
   "\n"
   ;; Container PID 1 is `claude-container-startup.sh' — it registers
   ;; the anvil MCP in ~/.claude.json, brings up the in-container emacs
   ;; daemon (unless --no-anvil), sources per-session hooks, then execs
   ;; claude (or bash under --shell).  We prepend `env GUIX_LOCPATH=…'
   ;; so container bash's setlocale() succeeds without \"cannot change
   ;; locale\" warnings.
   "guix_cmd=(\n"
   "  guix shell \"${opts[@]}\" \"${extra_opts[@]}\"\n"
   "  --expose=" claude-container-startup "\n"
   "  -m " claude-container-manifest "\n"
   "  \"${session_pkgs[@]}\"\n"
   "  --\n"
   "  env \"${container_env[@]}\"\n"
   "     bash " claude-container-startup " \"${claude_args[@]}\"\n"
   ")\n"
   "\n"
   "if [ \"$needs_cleanup\" = yes ]; then\n"
   "  \"${guix_cmd[@]}\"\n"
   "  exit $?\n"
   "else\n"
   "  exec \"${guix_cmd[@]}\"\n"
   "fi\n"))

(define-public claude-container
  (package
    (name "claude-container")
    (version "0.3.0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((out    #$output)
                 (bin    (string-append out "/bin"))
                 (script (string-append bin "/claude"))
                 ;; Standard bash-completion search path.  Nothing on
                 ;; this host runs bash-completion's lazy loader, so
                 ;; (peteches home modules base) sources this file
                 ;; directly from ~/.guix-home/profile; the location is
                 ;; still the conventional one in case that changes.
                 (comp    (string-append out "/share/bash-completion/completions")))
            (mkdir-p bin)
            (copy-file #$claude-container-wrapper script)
            (chmod script #o755)
            (mkdir-p comp)
            (copy-file #$(local-file (source-path "configs/claude/claude-completion.bash"))
                       (string-append comp "/claude"))))))
    (synopsis "Launch claude-code inside a guix shell --container")
    (description
     "Wrapper package that runs claude-code inside an isolated
@code{guix shell --container}.

@code{--session NAME} persists container state (Claude Code config,
Emacs daemon dir, npm cache, XDG dirs, bash history) under
@file{~/.claude-sessions/NAME/} so multiple logged-in Anthropic
sessions can coexist on a single machine.  New sessions are seeded
from the host's @file{~/.claude} and @file{~/.claude.json}.  Legacy
session dirs can be converted with @option{--migrate y}.

Each session may include: a @file{shared-dirs} file whose first line
is the container's CWD and whose lines are added as @option{--share}
bind-mounts; an @file{init.sh} sourced after anvil starts; an
@file{env} file whose @code{KEY=VALUE} lines are auto-exported; and
a @file{claude-overrides.json} recursively merged over the host's
@file{~/.claude/settings.json} at every launch so global
@code{mcpServers} and hook edits propagate without session
recreation.

A session can also extend the container manifest without editing it:
@file{packages} lists extra package specifications (whitespace- or
newline-separated, @code{#} comments allowed) that are added to the
environment, and @file{manifest.scm} is passed as an additional
@option{--manifest} for cases needing package transformations or
custom package definitions.  Both are additive, so a Go project's
session needs only a @file{packages} file containing @code{go},
@code{gopls} and @code{delve}.

Secrets are a separate channel from the plaintext @file{env} file,
carried by sops-encrypted dotenv files (safe to commit, since they are
ciphertext).  Two are read, in order: @file{~/.claude/secrets.env},
shared by every session, then the session's own @file{secrets.env}.
Both are sourced under @code{set -a}, so a session key overrides the
shared one of the same name; homelab-wide credentials such as
@env{GRAFANA_SERVICE_ACCOUNT_TOKEN} belong in the shared file, where
rotating one is a single-file edit.  The wrapper runs @code{sops -d} on
the host, where gpg-agent lives, into a private @file{/dev/shm} file
exposed read-only into the container and shredded on exit, so decrypted
values never reach the session dir, the store, or the @code{guix}
command line.

The Grafana MCP server is registered automatically in every session
when @env{GRAFANA_URL} is set (it defaults to the Tailscale-fronted
vhost).  It is a single server covering Prometheus metrics, Loki logs
and Grafana dashboards, restricted to a read-only tool set.

@option{--worktree [BRANCH]} provisions a git worktree of @code{$PWD}
and uses it as CWD, letting several claude sessions work on the same
repo in parallel without contention.  This is the DEFAULT; pass
@option{--no-worktree} to opt out.  If @code{$PWD} is not a git
working tree the wrapper falls back silently to no-worktree unless
@option{--worktree} was passed explicitly.  @option{--rm} uses an
ephemeral mktemp session.  @option{--shell} drops into container
bash.  @option{--no-anvil} skips the emacs daemon for fast one-off
runs.

An @code{emacs-no-x} daemon hosts the Anvil MCP server
(https://github.com/zawatton/anvil.el), packaged as
@code{emacs-anvil} so no network access is needed at container
startup.  On every launch the wrapper rewrites the anvil MCP entries
in @file{~/.claude.json} with container-resolved paths, so a host
@code{guix gc} cannot orphan the seeded config.")
    (home-page "https://github.com/peteches")
    (license license:gpl3+)))
