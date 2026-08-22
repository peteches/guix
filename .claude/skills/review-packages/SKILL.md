# review-packages

Report-only version-freshness check for the custom packages this repo
actually deploys. This is the package half of the scheduled dependency
review — it **never edits any file, never touches a `sha256`, and never
triggers a rebuild.** Detection only.

**Usage:**
- `review-packages` — check every deployed, non-dependency-closure package
  against its latest upstream version

This is a repository-only skill. Run it from the repo root (`~/area_51/guix`).

---

## Step 1 — Build the review scope: two reachability paths, minus `*-deps.scm`

A single-hop grep of `(peteches packages X)` against `peteches/systems/`,
`peteches/home/`, and `peteches/services/` is **not enough**, for two
independent reasons — miss either one and real, deployed packages get
reported as orphaned:

- **Multi-hop module imports.** Several packages are only reachable
  multiple hops deep through other package files (e.g. `seerr.scm` →
  `seerr-deps.scm` → `mermaid.scm` → `mermaid-deps.scm`; `comfyui-mcp.scm`
  → `claude-agent-acp.scm` → `claude-agent-acp-deps.scm`).
- **Dynamic name-string lookups.** `(specification->package "name")` (and
  `specification->package+output`) resolve a package by its `name` field at
  *runtime*, against every package Guix can find on `%package-module-path`
  — no `#:use-module (peteches packages X)` line exists at the call site at
  all, so a pure import-graph walk will silently miss it. This is a real,
  common pattern here: confirmed live in `peteches/home/modules/base.scm`
  (20+ specs including `"beeper-bin"`, `"feishin-bin"`, `"emacs-slack"`,
  `"emacs-mcp"`), `peteches/systems/base.scm`, and several
  `peteches/home/services/*.scm` / `peteches/services/*.scm` files. A file
  that looked fully orphaned by import-graph analysis alone
  (`peteches/packages/beeper.scm`, `feishin.scm`, `emacs.scm` were all
  flagged this way on 2026-08-21, then corrected) can still be deployed
  through this path.

1. **Import-graph seed and closure** (as before): seed with `grep -rl
   "peteches packages" peteches/systems peteches/home peteches/services`,
   then transitively follow each reached `peteches/packages/X.scm` file's own
   `#:use-module (peteches packages Y)` lines until a pass adds nothing new.
2. **Dynamic-lookup pass:** `grep -rn "specification->package"
   --include="*.scm" .` across the **whole repo** (not just
   systems/home/services — `peteches/services/*.scm` matters here too), then
   pull every quoted string literal out of each match's surrounding
   `specification->package`/`specification->package+output` call (these are
   often multi-line lists — read enough context around each match to get
   every string in the list, not just the first). Strip any `@version`
   suffix (e.g. `"awscli@2.28.0"` → `awscli`) before comparing, since specs
   can pin a version that way.
3. **Resolve dynamic-lookup strings to files, by evaluation, not grep.** For
   each candidate `peteches/packages/*.scm` file (everything not yet ruled
   in by the import-graph pass), load it and evaluate `(package-name
   <binding>)` for each `define-public` — do **not** grep for a literal
   `(name "...")` field, because some bindings don't have one in the
   defining file at all (e.g. `emacs.scm`'s `emacs-slack`, or
   `shims.scm`'s `python-six-bootstrap`, both defined via
   `hidden-package`/inheritance wrappers where the name comes from the
   wrapped package, not a literal in this file — the version-idiom lesson
   from Step 2 applies here too). If any evaluated name matches one of the
   Step 1.2 spec strings, that file (and just that specific binding, not
   necessarily every binding in the file) is in scope.
4. Drop every `*-deps.scm` file from the combined set before reviewing.
   These are auto-generated npm/go/python dependency closures pinned as a
   unit against a lockfile — there is no meaningful "check for updates" on
   an individual binding inside one; the thing worth version-checking is
   the culminating product package in the file that imports it (already
   caught above, since the product file is what's directly reachable from
   configs, from another product file, or from a spec string). If
   regenerating a closure is ever needed, that's a manual `guix import
   npm`/`guix import go`/`guix import pypi` re-run, not something this
   skill does.
5. Whatever remains unreached by *both* passes is genuinely orphaned.
   Re-derive this every run — do not trust a cached list from a previous
   report, and do not stop at the import-graph pass alone.

The result is the final list of `.scm` files to review. For a file reached
via the import-graph pass (1), review every `define-public` binding it
contains — after the product/deps split, product files are intentionally
minimal, so this stays accurate. For a file reached *only* via the
dynamic-lookup pass (2-3), review just the specific binding(s) whose
evaluated `package-name` matched a spec string — a file can define bindings
that are never looked up by name anywhere (e.g. `emacs.scm`'s
`emacs-linear`, confirmed unreferenced as of 2026-08-21 even though
`emacs-slack` and `emacs-mcp` in the same file are), and those stay out of
scope rather than being swept in just because a sibling binding is used.

---

## Step 2 — Extract name/version/origin via evaluation, not regex

`peteches/packages/*.scm` uses at least three different version-authoring
idioms — inline literal `(version "1.2.3")`, a `let*`-bound `version`, and a
top-level `(define %version "1.2.3")` referenced from inside the package
form. A regex for `(version "...")` will miss or misattribute some of these.
Evaluate instead.

For each file in the Step 1 scope, in **one fresh `guix repl -L .` process
per file** (never share a process across files — a module that fails to
load can leave a stub that makes a later `resolve-interface` in the *same*
process spuriously succeed; see this repo's CLAUDE.md):

1. `(use-modules (peteches packages X))`
2. Find the binding names in that file with a plain `grep -n
   "^(define-public"` (regex is fine for *names* — it's only version
   *values* that vary by idiom and need evaluation).
3. For each name, evaluate and record:
   - `(package-name <binding>)`
   - `(package-version <binding>)`
   - `(package-source <binding>)` → walk to `(origin-method ...)` and
     `(origin-uri ...)` (the `uri` may be a string, a list of strings, or a
     procedure for `git-fetch`/`svn-fetch`/etc. — handle each case; for
     `git-fetch`/`svn-fetch` the interesting field is inside `(git-reference
     ...)` / similar, not `origin-uri` directly)
   - `(package-home-page <binding>)`

This gives every reviewed package's current version and enough origin detail
for Step 4's dispatch, batched per file in one process — 30-40 short-lived
`guix repl` invocations total for the current scope, not one per binding.

---

## Step 3 — (folded into Step 1)

The `*-deps.scm` exclusion is handled in Step 1.4 above; nothing further to
do here. This step number is kept only so it lines up with the todo list —
skip straight to Step 4.

---

## Step 4 — Per-origin checker dispatch

Classify each reviewed package's origin from Step 2's evaluated data (never
regex-guess this) and pick a checker, in this order — stop at the first that
applies:

1. **Standard GitHub/PyPI shape.** Try `guix refresh -L . -t github,pypi -e
   '(@ (peteches packages X) binding-name)'`. If it reports a newer version,
   use it. If it reports "no updater" or errors, fall through.
2. **Other git-fetch, or a GitHub/Codeberg/GitLab tarball URL that
   `guix refresh` didn't match.** `git ls-remote --tags <repo-url>` and
   compare the highest tag against the current version (same technique as
   the `update-channels`/`review-channels` skills — no cloning required).
   **Tags are not returned in version order** — `git ls-remote --tags`
   lists them in ref order (roughly creation/alphabetical), not semver
   order, so do not eyeball the last few lines of raw output as "the
   latest" (confirmed live 2026-08-22: doing exactly that on
   `github.com/jeffvli/feishin` and `github.com/anthropics/claude-code`
   both gave answers *lower* than the currently-pinned version — wrong).
   Strip any `v` prefix, keep only `X.Y.Z`-shaped refs, and sort with a
   real version-aware sort before taking the max, e.g.:
   `git ls-remote --tags <repo-url> | awk '{print $2}' | sed
   's#refs/tags/##;s/^v//' | grep -E '^[0-9]+\.[0-9]+\.[0-9]+$' | sort -V
   | tail -1`.
   If the repo has **no tags at all** (confirmed live case:
   `emacs-slack`/`emacs-mcp` in `emacs.scm`, both `git-fetch`-pinned to a
   raw commit with no upstream releases), tag comparison doesn't apply —
   fall back to `git ls-remote <repo-url> HEAD` and compare the commit
   hash against the pinned `commit` field instead (same "differs =
   update available" logic `review-channels` already uses).
3. **Known-source override table.** Some vendor-direct-download packages are
   *actually* built from a public, tagged source repo that neither the
   origin URL nor the `home-page` field mentions — `home-page` often points
   at a marketing site instead. Rather than guessing via `home-page` (case
   4) or giving up (case 5), check a small hand-verified table of
   `binding → real source repo` mappings first, and treat a hit here as an
   **exact match, same confidence as case 2** (not a proxy guess) — every
   entry was manually confirmed by comparing tag scheme against pinned
   version before being added:
   - `tailscale.scm` (`tailscale`, origin `pkgs.tailscale.com`) →
     `https://github.com/tailscale/tailscale` (confirmed 2026-08-22: tags
     match the pinned version exactly, e.g. pinned `1.102.2`, tag `v1.102.2`
     exists, latest tag at the time was `v1.102.3`)
   - `vault.scm` (`vault`, origin `releases.hashicorp.com`) →
     `https://github.com/hashicorp/vault` (confirmed 2026-08-22: same
     exact-match pattern, pinned `2.0.3`, latest tag `v2.0.4`)
   Run the Step 4.2 tag-check (with its version-sort fix) or the no-tags
   HEAD fallback against the override repo, whichever applies. Add an entry
   here only after manually confirming the tag/commit scheme actually lines
   up with this package's version field — an unverified guess belongs in
   case 4 instead, labeled as a guess, not silently added here.
4. **Home-page proxy** (no override-table entry, but `home-page` points at
   GitHub/Codeberg/GitLab). Run the Step 4.2 tag-check against *that* repo
   as an unverified proxy signal (label it "proxy check via home-page —
   verify manually before bumping", since the home-page repo isn't
   necessarily where the download artifact comes from — this is a guess,
   unlike case 3's confirmed mappings).
5. **Anything else.** Report `no automated check — manual review needed`.
   Confirmed case: `beeper.scm` (`beeper-bin`) — investigated 2026-08-22
   and stays here permanently (re-check only if the vendor's distribution
   changes): no public source repo (`git ls-remote` against plausible
   `github.com/beeper/*` repo names fails as if the repo doesn't exist),
   no electron-builder-style update manifest at the download host
   (`builds/latest.yml`, `builds/latest-linux.yml` both 404), and the
   `beeper.com/download` page is JS-rendered with no static download URL
   or version string to scrape from its HTML. Don't re-attempt the same
   probes on future runs without a reason to think something changed —
   record the negative result and move on.
   Do not block the rest of the report on this — keep going to the next
   package.

**Never** touch a package's `sha256`, run `guix refresh -u` (which edits
files), or trigger a build. This step only reads and compares version
strings.

---

## Step 5 — Report

Print one summary table covering every package in the Step 1 scope:

| Package | File | Current version | Latest found | Checker used | Confidence |
|---------|------|-----------------|--------------|--------------|------------|
| rclone | rclone.scm | `1.71.1` | `1.75.0` | guix refresh (github) | high |
| tailscale | tailscale.scm | `1.102.2` | `1.102.3` | known-source override (github.com/tailscale/tailscale) | high |
| vault | vault.scm | `2.0.3` | `2.0.4` | known-source override (github.com/hashicorp/vault) | high |
| emacs-slack | emacs.scm | `4c34c52` | `22ae94d` | HEAD vs pinned commit (no upstream tags) | high |
| claude-code | claude-code.scm | `2.1.235` | `2.1.239` | proxy via home-page (github.com/anthropics/claude-code) | verify manually |
| beeper-bin | beeper.scm | `4.2.948` | — | no automated check | manual review needed |

Real data from a live trial run on 2026-08-22 — see the entries this table
records in `docs/dependency-review-todo.org`'s history for the fuller
sample and how the tailscale/vault override-table entries and the
version-sort/no-tags fixes above were discovered.

`Confidence` should reflect how directly the checker's source maps to the
actual release artifact: `high` for the github/pypi updater, a tag match
against the exact repo the source is fetched from, a known-source override
hit, or a HEAD-vs-commit match; `verify manually` for a home-page-proxy
match; `manual review needed` when nothing automated ran.

Do **not** run `git add`, `git commit`, `git push`, edit any package file,
or touch any `sha256`. This skill is read-only end to end. If the report
shows something worth bumping, that's a manual edit (or a future
`/update-packages` apply skill, not yet built) — this skill only detects.
