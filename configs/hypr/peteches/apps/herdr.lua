;; ── herdr ──────────────────────────────────────────────────────────────────
;; herdr-mx lives on dagon as a git checkout (~/area_51/herdr-mx, cargo build
;; --profile release-debug) plus the herdr-mx-pull + herdr-mx-remote-scan
;; systemd user services, which are NOT in this file's autostart because they
;; are systemd units — see "Remote workflow" in ~/area_51/herdr-mx/AGENTS.md.
;; The pinned commit the checkout is held at is the herdr-mx package's
;; source-pin in the Guix repo — keep them moving together.
;;
;; The SERVER now lives on "agents" (192.168.51.208): the system shepherd
;; supervises it as herdr-server-peteches (peteches/services/herdr.scm) and the
;; binary comes from agents' HOME profile, so `herdr --remote agents` from any
;; dagon terminal attaches — verified working on 2026-11-05. The retired
;; claude-workstation server is gone, and so is the ~/.local/bin copy that made
;; agents' failed 2026-11-03 attach probe say "herdr already installed but this
;; connection cannot use it" (that file was a broken test build; it now lives at
;; /var/tmp/herdr-selfprovision-residue/herdr.testbuild-2026-11-03 on agents).
;; See docs/herdr-mx-fix-testing-plan.org.
;;
;; Autostart stays commented: dagon's own client is started by the desktop
;; session, and a second client spawned here is the split-brain question in
;; docs/herdr-mx-fix-testing-plan.org §2. This line predates the server move and
;; still names the dead claude-workstation host; the live target would be
;; `--remote agents`, but enabling it has to wait for that split-brain decision.

# exec q terminal --maximize --class="herdr" -- herdr --remote claude-workstation
