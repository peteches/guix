# review-channels

Report-only freshness check across every `peteches/channels/*.scm` file. This
is the channel half of the scheduled dependency review — it **never edits
any file**, never touches commit pins, and never adds a channel
introduction. To actually apply an update, use `/update-channels`.

**Usage:**
- `review-channels` — check every pinned channel against its latest upstream commit

This is a repository-only skill. Run it from the repo root (`~/area_51/guix`).

---

## Step 1 — Parse all channel files

Read every `peteches/channels/*.scm` file and extract channel definitions. For
each file, collect every `(channel ...)` form that has a `url` field. Record:

| Field          | Notes                                                       |
|----------------|--------------------------------------------------------------|
| `name`         | symbol (e.g. `nonguix`)                                     |
| `url`          | git remote URL                                               |
| `branch`       | string; default `"master"` if the field is absent            |
| `commit`       | current pinned hash, or absent if the channel tracks HEAD    |
| `introduction` | present or absent                                             |
| `file`         | absolute path to the `.scm` file containing this entry        |

Because `nug.scm` re-exports channels from `base.scm` via `%base-channels`,
and `manual.scm` duplicates the same set again, a given `(url, branch)` pair
typically appears in **all three** files. Record every occurrence — the
report should show each file — but do not fetch per-occurrence (see Step 2).

---

## Step 2 — Fetch each unique (url, branch) exactly once

Build a cache keyed by the pair `(url, branch)`. For each **distinct** key
across all parsed entries — not per file, not per channel name — run:

```bash
git ls-remote <url> refs/heads/<branch>
```

Extract the 40-character hash from column 1. If that returns empty output,
fall back to:

```bash
git ls-remote <url> HEAD
```

Store the result in the cache under that `(url, branch)` key, then apply it
to **every** parsed entry (across all three channel files) that shares the
same key. With the current channel set this turns what would be up to ~15
`git ls-remote` calls (7 channels × up to 3 files each once `base.scm`,
`nug.scm`, and `manual.scm` are all counted) into at most 7 — one per
distinct channel. No cloning is needed for this step.

If a fetch fails (network error, unreachable remote), record the entry's
status as `fetch failed` and continue — do not abort the rest of the report.

---

## Step 3 — Check for missing channel introductions

For any channel entry with **no** `introduction` field, check once per
distinct `url` (introductions don't vary by branch) whether one is required:

```bash
BARE_DIR="/dev/shm/review-channels-<name>-intro-bare"
git clone --bare --depth=1 <url> "$BARE_DIR"
git --git-dir="$BARE_DIR" show HEAD:.guix-authorizations
rm -rf "$BARE_DIR"
```

- **File does not exist:** no introduction needed — nothing to report.
- **File exists:** flag it in the report as "introduction may be required —
  run `/update-channels` to get the exact commit hash and fingerprint to
  add." Do not compute the fingerprint or introduction block here — that is
  `/update-channels`'s job (Step 3 there) and requires the fuller unshallow
  history walk. This step stays a cheap yes/no check.

Always `rm -rf` the bare clone immediately after the check.

---

## Step 4 — Report

This skill makes **no edits**. Print one summary table covering every parsed
entry (all files, all channels):

| Channel | File | Current commit | Latest commit | Status |
|---------|------|----------------|----------------|--------|
| nonguix | base.scm | `bf39542c` | `bf39542c` | up to date |
| nonguix | nug.scm | `bf39542c` | `bf39542c` | up to date |
| nonguix | manual.scm | `bf39542c` | `bf39542c` | up to date |
| guix    | base.scm | `dd3e59ad` | `a1b2c3d4` | **update available** |

Use 8-character abbreviated hashes. `status` is one of: `up to date`,
`update available`, `fetch failed`, or `tracks HEAD` (channel has no pinned
`commit` field, e.g. `critical-grind` — still worth fetching HEAD to show in
the table, but there's no drift to flag).

Follow the table with a short list of any introduction concerns from Step 3.

Do **not** run `git add`, `git commit`, `git push`, or edit any
`peteches/channels/*.scm` file. This skill is read-only end to end — if the
report shows updates available, tell the user to run `/update-channels` to
apply them.
