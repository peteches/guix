# guix-update

Update all pinned channel commits in `peteches/channels/*.scm` to the latest commit on their respective branches, and verify channel introductions are present where available.

## Steps

For each channel defined in `peteches/channels/*.scm`:

1. **Read** the current channels file to extract each channel's `name`, `url`, `branch` (defaulting to `master` if absent), `commit`, and `introduction` (if present).

2. **Fetch the latest commit** for each channel by running:
   ```bash
   git ls-remote <url> refs/heads/<branch>
   ```
   Extract the commit hash from the output. If the channel has no explicit `branch` field, default to `master`. If that returns no output, fall back to `HEAD`:
   ```bash
   git ls-remote <url> HEAD
   ```

3. **Check for a channel introduction** — for any channel that lacks an `introduction` field, clone the repo into a temp directory and check for `.guix-authorizations`:
   ```bash
   git clone --bare <url> /tmp/<name>-bare
   git --git-dir=/tmp/<name>-bare show HEAD:.guix-authorizations
   ```
   If `.guix-authorizations` exists, the channel requires an introduction. Find the commit that first added it:
   ```bash
   git --git-dir=/tmp/<name>-bare log --oneline --diff-filter=A -- '.guix-authorizations'
   git --git-dir=/tmp/<name>-bare rev-parse <short-hash>
   ```
   The full hash of that commit is the introduction commit. The OpenPGP fingerprint comes from the `authorizations` s-expression in `.guix-authorizations`. Add the introduction to the channel definition:
   ```scheme
   (introduction
    (make-channel-introduction
     "<first-commit-hash>"
     (openpgp-fingerprint "<fingerprint>")))
   ```
   If `.guix-authorizations` does not exist, no introduction is needed.

4. **Update** `peteches/channels/*.scm` with the new commit hashes using the Edit tool, replacing each old `(commit "...")` value with the fetched latest commit for that channel.

5. **Validate** each updated channels file by running `guix time-machine` against it. Because our files are Guile modules (not bare channel lists), write a temporary wrapper that loads the module and extracts the exported variable via `module-ref`:

   ```bash
   printf '(load "/home/peteches/area_51/guix/peteches/channels/base.scm")\n(module-ref (resolve-module (quote (peteches channels base))) (quote %%base-channels))\n' \
     > /tmp/test-channels-base.scm
   guix time-machine -C /tmp/test-channels-base.scm -- describe
   ```

   Adapt for each channels file:
   - `peteches/channels/base.scm` → module `(peteches channels base)`, variable `%base-channels`
   - `peteches/channels/nug.scm` → module `(peteches channels nug)`, variable `%nug-channels`

   Use **absolute paths** in the `load` call — relative paths fail because `guix time-machine` does not run from the repo root.

   A successful run prints channel descriptions with their authenticated commit hashes. Any authentication failure, unknown-commit error, or missing-introduction warning indicates a problem to fix before proceeding.

6. Report a summary of what changed (old commit → new commit) for each channel, and flag any channel where the remote fetch failed or returned no result.
