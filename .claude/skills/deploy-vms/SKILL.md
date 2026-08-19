# deploy-vms

Deploy one or more VMs (or the whole fleet) via `scripts/deploy.scm`, one host
at a time, with a clear pass/fail summary at the end.

**Usage:**
- `deploy-vms` — deploy every machine in `peteches/machines.scm`, one at a time
- `deploy-vms <name> [<name> ...]` — deploy only the named machines
  (space- or comma-separated short names, e.g. `git`, `loki`, `prometheus`
  — see "Resolve the target host list" below)
- Anything after the names that isn't a recognised host name is forwarded
  to every `scripts/deploy.scm` invocation, e.g.
  `deploy-vms loki grafana --dry-run`

This is a repository-only skill. Run it from the repo root (`~/area_51/guix`).
`scripts/deploy.scm` already does the real work (host filtering, exec'ing
`guix deploy -L .`); this skill's job is turning a short, friendly host list
into safe, isolated, sequential invocations of it with a report at the end.

---

## Step 1 — Resolve the target host list

Get the authoritative machine list by parsing `peteches/machines.scm` at run
time — do not hardcode a copy of it here, it drifts (CLAUDE.md's "Adding a
New VM" step 4 already notes that `scripts/deploy.scm`'s own `%machine-names`
alist has to be kept in sync by hand; this skill should not add a second
place that can go stale):

```bash
grep -oP '(?<=\(define-public )\S+(?=-machine$)' peteches/machines.scm
```

This yields short names in file order: `prometheus`, `grafana`, `loki`,
`pihole`, `git`, `jellyfin`, `caddy`, `prowlarr`, `arr`, `downloads`,
`rustdesk`, `concourse-db`, `concourse-web01`, `concourse-worker01`, `vault`,
`critical-grind-outline`, `plane`, `critical-grind-campaign`,
`claude-workstation` (exact set may have grown — always re-run the grep,
never trust this list).

- **No arguments given:** target list = every name from the grep, in order.
- **Arguments given:** split on commas and/or whitespace, trim each token.
  For every token, check it appears verbatim in the grep output.
  - Any token that matches is a target host.
  - Any token that doesn't match anything is a typo — stop before deploying
    anything, tell the user which token(s) didn't resolve, and suggest the
    closest name(s) from the list. Don't silently drop it or pass it through
    to `guix deploy` as a raw regex — a typo'd `--hosts` pattern can silently
    match zero machines (deploy.scm already guards that case) or, worse,
    accidentally match a machine nobody intended via substring overlap.
  - A token that isn't meant as a host name at all (starts with `-`, e.g.
    `--dry-run`) is passthrough, not a host — collect these separately and
    forward them to every invocation in Step 3.

## Step 2 — Check each host is up before deploying it

For every resolved name, get its Tailscale MagicDNS `host-name` from
`peteches/machines.scm` — this is what `guix deploy` actually connects to,
so it's what must be probed, not the VM's LAN IP:

```bash
grep -A6 "(define-public <name>-machine\$" peteches/machines.scm | grep -oP '(?<=\(host-name ")[^"]+'
```

(By repo convention every entry's `host-name` is `<name>.spaniel-cordylus.ts.net`
— see the comment block at the top of `machines.scm` — but extract it rather
than assume it, in case a future machine breaks the pattern.)

Then probe TCP/22 with a short timeout — this is the exact prerequisite
`guix deploy` needs (SSH reachability), so it's a more meaningful check than
ICMP, which the VM's firewall may or may not treat the same way:

```bash
timeout 3 bash -c "echo > /dev/tcp/<host-name>/22" 2>/dev/null && echo UP || echo DOWN
```

- **UP:** proceed to Step 3 for this host.
- **DOWN:** do **not** attempt `scripts/deploy.scm` for this host — record it
  as skipped (host unreachable) and move on to the next host in the list.
  Don't let one unreachable VM block the rest, and don't waste time letting
  `guix deploy` itself time out on a host this quick probe already ruled out.

## Step 3 — Deploy each reachable host, one at a time

For each host that passed Step 2, in order:

```bash
./scripts/deploy.scm --hosts "name=^<name>-machine\$" [passthrough-args...]
```

Use the anchored `name=^<name>-machine$` form, **not** a bare `--hosts <name>`.
`deploy.scm`'s bare-pattern matching is an unanchored substring regex against
both `name` and `host-name` — a bare `--hosts arr` also matches `prowlarr`
(`"arr"` is a substring of `"prowlarr"`), silently deploying a second machine
nobody asked for. This isn't hypothetical: it happened during this skill's
own testing. Anchoring against the exact `<name>-machine` variable name (the
string `scripts/deploy.scm`'s own `%machine-names` alist maps each record
to) is the only pattern form that's guaranteed to select exactly one machine.

Run these **sequentially, never in parallel** — they share the local
`guix-daemon` and substitute cache, and interleaved output from concurrent
`guix deploy` runs is unreadable and can thrash the daemon. Capture the exit
status of each invocation.

If one host fails, record the failure and **continue to the next host** —
do not abort the rest of the list. This is the whole reason to loop
per-host rather than pass the full list to a single
`--hosts name1,name2,name3` call: one bad machine is far easier to isolate
and diagnose in its own invocation's output than buried inside a combined
`guix deploy` run across several machines, and a failure early in a combined
run can prevent later machines in the same run from being attempted at all.

## Step 4 — Report

Print a summary table covering every host in the target list, including ones
skipped at Step 2:

| Host | Result |
|------|--------|
| prometheus | deployed |
| loki | deployed |
| git | **FAILED** |
| pihole | skipped — host unreachable (port 22 timed out) |

For any failure, include the tail of that host's `guix deploy` output (the
actual error) directly under the table — not just the word "FAILED". For any
argument that failed to resolve in Step 1, list it separately as "not
deployed — unrecognised host" so it's clear those were never attempted.

---

## Notes

- This runs real `guix deploy` against live systems — treat it with the same
  care as any other action with a large blast radius. Naming specific hosts
  is the user's confirmation for those hosts. Running with **no arguments**
  deploys the entire fleet in one go; if that wasn't clearly what the user
  asked for (e.g. they just said "run the deploy skill" ambiguously rather
  than "deploy everything"/"deploy all the VMs"), confirm the full-fleet
  scope with them before starting Step 2 (the liveness checks) — probing
  every VM's SSH port is harmless either way, but don't go on to actually
  deploy without that confirmation.
- `guix deploy -L .` (what `scripts/deploy.scm` execs) reads whatever is
  currently on disk in the working tree, not a pushed git ref. If the repo
  has uncommitted or unpushed changes relevant to the hosts being deployed,
  that's expected and intentional when deploying local edits — but if it
  looks accidental (e.g. unrelated in-progress changes to unrelated files),
  mention it before proceeding rather than deploying it silently.
- Do not run `git add`, `git commit`, or `git push` as part of this skill —
  deploying and committing are separate actions; leave staging/committing to
  the user unless they separately ask for it.
