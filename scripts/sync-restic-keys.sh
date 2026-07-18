#!/usr/bin/env bash
set -euo pipefail

# sync-restic-keys.sh — publish every VM's restic SSH public key to the NAS.
#
# Connects AS restic-backup, not as a human operator.  That is deliberate and
# load-bearing.  Synology's sshd refuses an authorized_keys that any user other
# than its owner can write, and DSM grants an explicit ACL entry to whoever
# creates the file.  Uploading as peteches therefore left
#
#     [0] user:peteches:allow:rwxpdDaARWcCo
#
# on the file, and sshd then rejected every key in it with a bare
# "Permission denied (publickey,password)" -- no log line, no clue -- silently
# disabling backups on every VM at once.  Writing as the owner leaves no
# foreign ACE, so the failure cannot recur.
#
# The operator's own public key is appended to the uploaded file precisely so
# that this works: it is what lets a human log in as restic-backup to run the
# sync at all.  Override with OPERATOR_PUBKEY.
#
# BOOTSTRAP.  The very first run cannot connect as restic-backup, because the
# operator key is not in the file yet.  Do one run as an account with write
# access, then strip the ACE it leaves behind:
#
#     SYNOLOGY_USER=peteches scripts/sync-restic-keys.sh
#     ssh peteches@nas.peteches.co.uk \
#       'sudo synoacltool -del /volume1/homes/restic-backup/.ssh/authorized_keys 0'
#     ssh peteches@nas.peteches.co.uk \
#       'sudo synoacltool -get /volume1/homes/restic-backup/.ssh/authorized_keys'
#
# The last command must show only the restic-backup entry.  Every later run
# uses the default user and needs no repair.
#
# This script REPLACES the NAS authorized_keys wholesale, so a host whose key
# fails to decrypt would be silently revoked.  It refuses to upload at all if
# any extraction failed, rather than uploading a partial file.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
SECRETS_DIR="$REPO_ROOT/secrets/hosts"

SYNOLOGY_HOST="${SYNOLOGY_HOST:-nas.peteches.co.uk}"
SYNOLOGY_USER="${SYNOLOGY_USER:-restic-backup}"
OPERATOR_PUBKEY="${OPERATOR_PUBKEY:-$HOME/.ssh/id_ed25519.pub}"
# Relative to the login user's home, so this does not depend on whether /homes
# or /volume1/homes is canonical on the NAS -- they differ, and the absolute
# form only worked by accident of the symlink.
AUTHORIZED_KEYS_PATH="${AUTHORIZED_KEYS_PATH:-.ssh/authorized_keys}"

KEYS_FILE="/dev/shm/restic-keys-$$"
SFTP_BATCH="/dev/shm/restic-sftp-batch-$$"
trap 'shred -u "$KEYS_FILE" "$SFTP_BATCH" 2>/dev/null || true' EXIT

if [[ ! -r "$OPERATOR_PUBKEY" ]]; then
    echo "Error: operator public key not readable: $OPERATOR_PUBKEY" >&2
    echo "Set OPERATOR_PUBKEY to point at yours." >&2
    exit 1
fi

collected=0
failed=()

for secret_file in "$SECRETS_DIR"/*/restic.yaml; do
    host=$(basename "$(dirname "$secret_file")")
    # pipefail is on, so a failed decrypt fails the whole pipeline rather than
    # being masked by sed's exit status.
    if ssh-keygen -y -f <(sops --decrypt --extract '["ssh-key"]' "$secret_file") \
            2>/dev/null \
            | sed -e "s/@.*/@${host}.peteches.co.uk/" >> "$KEYS_FILE"; then
        echo "Collected key for $host"
        collected=$((collected + 1))
    else
        failed+=("$host")
        echo "Warning: failed to extract key for $host" >&2
    fi
done

if (( ${#failed[@]} > 0 )); then
    echo >&2
    echo "Error: refusing to upload. Extraction failed for: ${failed[*]}" >&2
    echo "This script replaces authorized_keys wholesale, so uploading now" >&2
    echo "would revoke backup access for those hosts -- and nothing would" >&2
    echo "report it until a restore was needed. Fix the secrets and re-run." >&2
    exit 1
fi

if (( collected == 0 )); then
    echo "Error: no keys collected" >&2
    exit 1
fi

# Appended last, and without the comment rewrite applied to host keys: this one
# identifies a person, not a machine.
printf '%s\n' "$(cat "$OPERATOR_PUBKEY")" >> "$KEYS_FILE"
echo "Added operator key from $OPERATOR_PUBKEY"

echo "Syncing $collected host keys + operator key to ${SYNOLOGY_USER}@${SYNOLOGY_HOST}:${AUTHORIZED_KEYS_PATH}"

printf 'put %s %s\nchmod 600 %s\n' \
    "$KEYS_FILE" "$AUTHORIZED_KEYS_PATH" "$AUTHORIZED_KEYS_PATH" \
    > "$SFTP_BATCH"
sftp -b "$SFTP_BATCH" "${SYNOLOGY_USER}@${SYNOLOGY_HOST}"

echo "Done."
echo
echo "Confirm no foreign ACL entry was created -- only restic-backup should be listed:"
echo "  ssh ${SYNOLOGY_USER}@${SYNOLOGY_HOST} \\"
echo "    'synoacltool -get /volume1/homes/restic-backup/.ssh/authorized_keys'"
echo
echo "Then confirm a VM can actually reach the repository, e.g.:"
echo "  ssh critical-grind-campaign 'sudo herd trigger restic-backup'"
