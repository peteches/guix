#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
SECRETS_DIR="$REPO_ROOT/secrets/hosts"
SYNOLOGY_HOST="${SYNOLOGY_HOST:-nas.peteches.co.uk}"
SYNOLOGY_USER="${SYNOLOGY_USER:-peteches}"
AUTHORIZED_KEYS_PATH="/homes/restic-backup/.ssh/authorized_keys"

KEYS_FILE="/dev/shm/restic-keys-$$"
SFTP_BATCH="/dev/shm/restic-sftp-batch-$$"
trap 'shred -u "$KEYS_FILE" "$SFTP_BATCH" 2>/dev/null || true' EXIT

for secret_file in "$SECRETS_DIR"/*/restic.yaml; do
    host=$(basename "$(dirname "$secret_file")")
    if ssh-keygen -y -f <(sops --decrypt --extract '["ssh-key"]' "$secret_file") \
            2>/dev/null \
            | sed -e "s/@.*/@${host}.peteches.co.uk/" >> "$KEYS_FILE"; then
        echo "Collected key for $host"
    else
        echo "Warning: failed to extract key for $host" >&2
    fi
done

KEY_COUNT=$(wc -l < "$KEYS_FILE")
if [[ "$KEY_COUNT" -eq 0 ]]; then
    echo "Error: no keys collected" >&2
    exit 1
fi

echo "Syncing $KEY_COUNT SSH public keys to $SYNOLOGY_USER@$SYNOLOGY_HOST:$AUTHORIZED_KEYS_PATH"

printf 'put %s %s\nchmod 600 %s\n' \
    "$KEYS_FILE" "$AUTHORIZED_KEYS_PATH" "$AUTHORIZED_KEYS_PATH" \
    > "$SFTP_BATCH"
sftp -b "$SFTP_BATCH" "$SYNOLOGY_USER@$SYNOLOGY_HOST"

echo "Done."
