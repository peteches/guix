# review-dependencies

Single entry point for the scheduled dependency review: runs the channel and
package checks and merges their results into one report. This is what the
monthly cron routine calls — it is **report-only end to end**, same as the
two skills it wraps: no file edits, no `git add`/`commit`/`push`, no
`sha256` touches, no rebuilds. Applying anything found stays a manual step
(`/update-channels` for channels; there is no `/update-packages` apply skill
yet — bumping a package version is a manual edit for now).

**Usage:**
- `review-dependencies` — run both checks and report a merged summary

This is a repository-only skill. Run it from the repo root (`~/area_51/guix`).

---

## Step 1 — Run review-channels

Invoke `review-channels` (`.claude/skills/review-channels/SKILL.md`) in
full. Keep its per-channel results: name, file, current commit, latest
commit, status, and any introduction concerns.

## Step 2 — Run review-packages

Invoke `review-packages` (`.claude/skills/review-packages/SKILL.md`) in
full. Keep its per-package results: name, file, current version, latest
found, checker used, confidence.

Steps 1 and 2 are independent — nothing in either depends on the other's
result, so if one is slow (e.g. `review-packages` making ~35 `guix repl`
calls) it does not block starting the other first if running interactively.

## Step 3 — Merge into one table

Both skills already do their own dedup/scoping internally (channel fetches
deduped by `(url, branch)`; packages scoped to the transitive, non-`*-deps`
reachable set) — this step only concatenates their already-computed rows
into one shared schema, it does not re-derive or re-check anything:

| Item | Kind | File | Current | Latest | Source | Confidence |
|------|------|------|---------|--------|--------|------------|
| nonguix | channel | base.scm | `bf39542c` | `bf39542c` | git ls-remote | up to date |
| guix | channel | base.scm | `dd3e59ad` | `a1b2c3d4` | git ls-remote | **update available** |
| tailscale | package | tailscale.scm | `1.102.2` | `1.104.0` | proxy via home-page | verify manually |
| node-mermaid-js-mermaid-cli | package | mermaid.scm | `11.12.0` | — | no automated check | manual review needed |

Column mapping from each skill's own report:
- Channels: `Item`=channel name, `Current`/`Latest`=commit (8-char abbreviated),
  `Source`=`git ls-remote`, `Confidence`=the channel skill's `Status` column
  verbatim (`up to date` / `update available` / `fetch failed` / `tracks HEAD`).
- Packages: `Item`=package binding name, `Current`/`Latest`=version string,
  `Source`=the package skill's `Checker used` column, `Confidence`=the
  package skill's `Confidence` column verbatim.

Sort the merged table with anything actionable first: `update available` /
`verify manually` rows before `up to date` / `high` confidence rows, and put
`fetch failed` / `manual review needed` rows last (nothing to act on
automatically, but still worth a human glance eventually).

Append review-channels' introduction concerns (if any) as a short separate
list after the table, same as that skill reports them standalone.

## Step 4 — Deliver

Per how this skill is invoked:
- **Interactively** (someone runs `/review-dependencies` directly): print the
  merged table and introduction-concerns list as the response.
- **From the scheduled monthly cron routine**: send the same merged table as
  a message/summary back to the user — this skill does not write the report
  to any file in the repo, and does not open a PR, issue, or commit. See
  "Wire up monthly schedule" in `docs/dependency-review-todo.org` for how
  the cron routine itself is set up; this skill is only the thing it calls.

Do **not** run `git add`, `git commit`, `git push`, edit any channel or
package file, or touch any `sha256`, in this skill or in either skill it
wraps. If the report shows updates available, tell the user to run
`/update-channels` (channels) or make a manual package-version edit
(packages) — this skill only detects and reports.
