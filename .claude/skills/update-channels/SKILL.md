# update-channels

Update pinned channel commits in all `peteches/channels/*.scm` files, and detect any missing channel introductions.

**Usage:**
- `update-channels` — update each channel to its latest commit
- `update-channels <N>` — update each channel to its most recent commit that is at least N days old

This is a repository-only skill. Run it from the repo root (`~/area_51/guix`).

---

## Step 1 — Parse all channel files

Read every `peteches/channels/*.scm` file and extract channel definitions. For each file, collect every `(channel ...)` form that has a `url` field. Record:

| Field          | Notes                                                         |
|----------------|---------------------------------------------------------------|
| `name`         | symbol (e.g. `nonguix`)                                       |
| `url`          | git remote URL                                                |
| `branch`       | string; default `"master"` if the field is absent            |
| `commit`       | current pinned hash, or absent if the channel tracks HEAD    |
| `introduction` | present or absent                                             |
| `file`         | absolute path to the `.scm` file containing this entry       |

Because `nug.scm` re-exports channels from `base.scm` via `%base-channels`, a given (name, url) pair may appear in multiple files. Track the file path per entry so each occurrence is updated independently.

---

## Step 2 — Fetch the target commit for each channel

Parse the numeric argument to get `MIN_AGE_DAYS` (default `0`).

### Case A — `MIN_AGE_DAYS = 0` (latest commit)

```bash
git ls-remote <url> refs/heads/<branch>
```

Extract the 40-character hash from column 1. If that returns empty output, fall back to:

```bash
git ls-remote <url> HEAD
```

Record the hash as the new commit. No cloning required.

### Case B — `MIN_AGE_DAYS > 0` (commit at least N days old)

A shallow clone is needed because `git ls-remote` only returns the current tip.

1. Use `/dev/shm` — **never `/tmp`**:
   ```bash
   BARE_DIR="/dev/shm/update-channels-<name>-bare"
   ```

2. Clone with enough history to cover N+14 days:
   ```bash
   SINCE=$(date -d "-$((MIN_AGE_DAYS + 14)) days" '+%Y-%m-%d')
   git clone --bare --shallow-since="$SINCE" --single-branch --branch <branch> <url> "$BARE_DIR"
   ```

3. Find the most recent commit at least N days old:
   ```bash
   CUTOFF=$(date -d "-${MIN_AGE_DAYS} days" '+%Y-%m-%d %H:%M:%S')
   NEW_COMMIT=$(git --git-dir="$BARE_DIR" log --before="$CUTOFF" -1 --format="%H")
   ```
   If `NEW_COMMIT` is empty, extend the history window and retry:
   ```bash
   git --git-dir="$BARE_DIR" fetch --shallow-since="$(date -d "-$((MIN_AGE_DAYS + 60)) days" '+%Y-%m-%d')"
   NEW_COMMIT=$(git --git-dir="$BARE_DIR" log --before="$CUTOFF" -1 --format="%H")
   ```
   If still empty, skip this channel and report a warning.

4. Remove the bare clone immediately — it contains only public commit objects, no secrets:
   ```bash
   rm -rf "$BARE_DIR"
   ```

---

## Step 3 — Check for missing channel introductions

For any channel that has **no** `introduction` field, check whether one is required.

Clone the repo into `/dev/shm` (bare, single-commit depth is enough to check):

```bash
BARE_DIR="/dev/shm/update-channels-<name>-intro-bare"
git clone --bare --depth=1 <url> "$BARE_DIR"
```

Check for `.guix-authorizations`:

```bash
git --git-dir="$BARE_DIR" show HEAD:.guix-authorizations
```

- **If the file does not exist:** no introduction needed. Continue.
- **If it exists:** an introduction is required. Find the commit that first added it:
  ```bash
  # Need fuller history for the log search
  git --git-dir="$BARE_DIR" fetch --unshallow
  git --git-dir="$BARE_DIR" log --oneline --diff-filter=A -- '.guix-authorizations'
  git --git-dir="$BARE_DIR" rev-parse <short-hash>
  ```
  The full hash of that commit is the introduction commit. The OpenPGP fingerprint comes from the `authorizations` s-expression inside `.guix-authorizations`. Report the required introduction block to the user:
  ```scheme
  (introduction
   (make-channel-introduction
    "<first-commit-hash>"
    (openpgp-fingerprint "<fingerprint>")))
  ```
  Do not add the introduction automatically — report it and let the user add it manually, since fingerprint verification requires human judgement.

Remove the bare clone:
```bash
rm -rf "$BARE_DIR"
```

---

## Step 4 — Skip unchanged channels

If the fetched `NEW_COMMIT` equals the channel's current `commit` value (or the channel has no `commit` field), skip it. Do not write a no-op edit.

---

## Step 5 — Update commit hashes in each file

For every channel where the new commit differs from the pinned one, replace the old hash using `mcp__anvil-emacs-eval__file-replace-string` (one call per channel per file):

```
old: (commit "<current-hash>")
new: (commit "<new-hash>")
```

If multiple channels in the same file need updating, collapse them into a single `mcp__anvil-emacs-eval__file-batch` call.

After all replacements in a file, verify paren balance:

```elisp
(with-current-buffer (find-file-noselect "/abs/path/to/file.scm")
  (check-parens))
```

Treat any `check-parens` error as a blocker — do not save until resolved.

---

## Step 6 — Validate updated files

For each modified file, confirm Guile can still load it.

**Module files** (`base.scm`, `nug.scm`):
```bash
guile -L /home/peteches/area_51/guix -c '(use-modules (peteches channels base))'
guile -L /home/peteches/area_51/guix -c '(use-modules (peteches channels nug))'
```

**Plain-list files** (`manual.scm`, `channels-nug.scm`):

These files use Guix record macros (`channel`, `make-channel-introduction`, etc.) and cannot be evaluated by bare `guile` — they require the full Guix environment. Validate them with a paren-balance check only (already done in Step 5), plus a read-only syntax parse:

```bash
guile -c '(call-with-input-file "/home/peteches/area_51/guix/peteches/channels/manual.scm"
            (lambda (port) (let loop () (let ((x (read port))) (unless (eof-object? x) (loop))))))'
guile -c '(call-with-input-file "/home/peteches/area_51/guix/peteches/channels/channels-nug.scm"
            (lambda (port) (let loop () (let ((x (read port))) (unless (eof-object? x) (loop))))))'
```

A zero exit confirms the file is syntactically valid Scheme (all s-expressions are well-formed and readable). Semantic validation (record field names, channel introductions) is covered by the module files — `base.scm` uses the same channel definitions and is loaded in a full Guix context.

---

## Step 7 — Report

Print a summary table of every channel processed:

| Channel | File | Old commit | New commit | Status |
|---------|------|-----------|-----------|--------|
| nonguix | base.scm | `bf39542c` | `a1b2c3d4` | updated |
| guix    | base.scm | `dd3e59ad` | `dd3e59ad` | unchanged |

Use 8-character abbreviated hashes. Call out any channel skipped due to fetch failure, insufficient history, or a missing introduction that needs manual attention.

Do **not** run `git add`, `git commit`, or `git push`. Leave all staging and committing to the user.
