# new-vm

Provision a new Proxmox QEMU/KVM VM into the Guix fleet end-to-end: clone the bootstrap
template, generate the system config, encrypt per-host secrets, deploy the static-IP
config, and register the VM in all supporting files.

## ⚠️ Secret handling — MANDATORY

**Never echo, print, or display any key material in shell command output.** This
conversation is recorded by Anthropic. Anything that appears in a Bash tool result
is permanently part of that record.

Prohibited from ever appearing in output:
- Private keys (SSH, age, GPG) and passwords — obvious secrets
- **Public keys and host keys** — expose VM identity and enable correlation attacks
- **Age public keys** — reveal which VMs can decrypt which SOPS secrets
- Any value read out of a SOPS-encrypted file

**Pattern to follow:**
- Store key material in shell variables or `/dev/shm` files — never print the value
- Confirm a step succeeded with a message like `echo "==> Host key collected."`, not
  `echo "==> Host key: $HOST_KEY"`
- Pass secrets between steps via variables or `/dev/shm` files, not via stdout
- `shred -u` every `/dev/shm` file as soon as it is no longer needed
- If a secret has appeared in output, treat it as compromised and rotate it immediately

## Parameters

| Parameter | Example | Notes |
|---|---|---|
| `<name>` | `jellyfin` | Short hostname — no domain suffix |
| `--cpus N` | `4` | vCPU count (optional, default 2) |
| `--memory MB` | `4096` | RAM in MB (optional, default 2048) |

The disk size is always the template default (25 G). No `--disk` parameter is accepted.

All of the following are **auto-detected** — do not prompt the user for them:
- **VMID** — query Proxmox `qm list`, take max ID in range 100–8999, add 1
- **IPv4** — scan `peteches/systems/pihole.scm` for `192.168.51.x` entries, take max + 1
  (`192.168.50.x` is the DHCP range and must never be assigned to a static VM)
- **IPv6** — scan all `peteches/systems/*.scm` for `2a10:d582:ef59::` addresses, take max + 1

## Security requirements

- **No plaintext secrets on disk.** Restic password and SSH private key must stay in shell
  variables or `/dev/shm` (Linux tmpfs — RAM only, never paged). The only thing written to
  disk from secret material is the final SOPS-encrypted file.
- `/tmp` is not acceptable for secret material.
- Use `shred -u` to immediately destroy any files created under `/dev/shm`.
- Pass secrets to Python subprocesses via environment variables, not as command-line arguments
  (argv is visible in `/proc/<pid>/cmdline`). `unset` them as soon as they are no longer needed.
- **GPG warm warnings.** Before every command that requires the GPG agent, print a boxed
  warning and `read` to pause. The user needs to warm their agent manually.

---

## Steps

### Phase 0 — Pre-flight: auto-discover IDs and confirm

1. **Discover the next free VMID** — filter to range 100–8999 to exclude the bootstrap
   template (VMID 9000) and any Proxmox system VMs below 100:
   ```bash
   NEXT_VMID=$(ssh root@proxmox1.peteches.co.uk qm list \
     | awk 'NR>1 && $1>=100 && $1<9000 {print $1}' \
     | sort -n | tail -1 \
     | xargs -I{} sh -c 'echo $(( {} + 1 ))')
   ```

2. **Allocate the next IPv4** address in the static subnet by scanning `pihole.scm`:
   ```bash
   NEXT_IPv4=$(python3 -c "
   import re
   with open('peteches/systems/pihole.scm') as f:
       addrs = [int(m) for m in re.findall(r'192\.168\.51\.(\d+)', f.read())]
   print(f'192.168.51.{max(addrs)+1}')
   ")
   ```

3. **Allocate the next IPv6** address by scanning all system configs (use Python to handle
   arbitrary hex suffixes safely):
   ```bash
   NEXT_IPv6=$(python3 -c "
   import re, glob
   addrs = [int(m, 16)
            for f in glob.glob('peteches/systems/*.scm')
            for m in re.findall(r'2a10:d582:ef59::([0-9a-f]+)', open(f).read())]
   print(f'2a10:d582:ef59::{hex(max(addrs)+1)[2:]}')
   ")
   ```

4. **Collision guard** — verify none of the auto-allocated values clash with existing configs:
   ```bash
   # IPv4 must not already appear in any .scm file
   grep -r "${NEXT_IPv4}" peteches/systems/ && { echo "ERROR: IPv4 ${NEXT_IPv4} already in use"; exit 1; } || true
   # IPv6 must not already appear in any .scm file
   grep -r "${NEXT_IPv6}" peteches/systems/ && { echo "ERROR: IPv6 ${NEXT_IPv6} already in use"; exit 1; } || true
   ```

5. **Print summary and pause for user confirmation** before touching anything:
   ```
   ┌────────────────────────────────────────────────────┐
   │  About to provision new VM                         │
   │  Name : <name>.peteches.co.uk                     │
   │  VMID : <NEXT_VMID>                               │
   │  IPv4 : <NEXT_IPv4>/23                            │
   │  IPv6 : <NEXT_IPv6>/64                            │
   └────────────────────────────────────────────────────┘
   Press Enter to continue or Ctrl+C to abort.
   ```
   ```bash
   read -r _confirm
   ```

---

### Phase 1 — Bootstrap the VM (takes several minutes while Phase 2 runs)

6. **Clone the template and boot the VM.** Capture the full output so the DHCP IP can be
   extracted; echo it to the terminal as it arrives:
   ```bash
   BOOTSTRAP_LOG=$(scripts/proxmox-clone-bootstrap \
     "$NEXT_VMID" "<name>" \
     [--cpus N] [--memory MB] \
     2>&1 | tee /dev/stderr; exit "${PIPESTATUS[0]}")
   DHCP_IP=$(echo "$BOOTSTRAP_LOG" | grep -oP '(?<=IP: )\S+')
   echo "==> VM DHCP IP: $DHCP_IP"
   ```
   The script saves `peteches/age-keys/<name>.pub` automatically.

   **⚠️ Age key already exists (re-run scenario):** If `peteches/age-keys/<name>.pub`
   already exists from a previous attempt, the bootstrap script exits early without
   starting the VM or printing the DHCP IP. Recover manually:
   ```bash
   # Check VM status and start if stopped
   ssh root@proxmox1.peteches.co.uk qm status "$NEXT_VMID"
   ssh root@proxmox1.peteches.co.uk qm start "$NEXT_VMID"

   # Poll for DHCP IP via guest agent
   NODE=$(ssh root@proxmox1.peteches.co.uk pvesh get /nodes \
     --output-format json | python3 -c \
     "import sys,json; print(json.load(sys.stdin)[0]['node'])")
   DHCP_IP=$(ssh root@proxmox1.peteches.co.uk \
     pvesh get "/nodes/$NODE/qemu/$NEXT_VMID/agent/network-get-interfaces" \
     --output-format json 2>/dev/null \
     | python3 -c "
   import sys, json
   for iface in json.load(sys.stdin).get('result', []):
       if iface.get('name') == 'eth0':
           for a in iface.get('ip-addresses', []):
               if a.get('ip-address-type') == 'ipv4':
                   print(a['ip-address'])
   " 2>/dev/null)
   # Do NOT echo DHCP_IP or the age key contents.
   [[ -n "$DHCP_IP" ]] && echo "==> DHCP IP obtained." \
     || { echo "ERROR: could not get DHCP IP"; exit 1; }
   # Read the pre-existing age key silently
   AGE_KEY=$(cat "peteches/age-keys/<name>.pub")
   [[ -n "$AGE_KEY" ]] && echo "==> Age key loaded." \
     || { echo "ERROR: age key file missing"; exit 1; }
   ```

7. **Collect the bootstrap SSH host key** (needed for Phase 5 deploy):
   ```bash
   BOOTSTRAP_HOST_KEY=$(ssh-keyscan -t ed25519 "$DHCP_IP" 2>/dev/null \
     | awk '{print $2" "$3}')
   # Do NOT echo BOOTSTRAP_HOST_KEY — host keys must not appear in conversation output.
   [[ -n "$BOOTSTRAP_HOST_KEY" ]] && echo "==> Bootstrap host key collected." \
     || { echo "ERROR: could not collect host key from $DHCP_IP"; exit 1; }
   ```

---

### Phase 2 — Create the system config

Create `peteches/systems/<name>.scm` using **Anvil/paredit** for all structural edits.
Model it on `peteches/systems/git.scm` (simplest current VM with alloy + tailscale).

The file must:

a. Declare its module and imports:
   ```scheme
   (define-module (peteches systems <name>)
     #:use-module (guix gexp)
     #:use-module (gnu bootloader)
     #:use-module (gnu bootloader grub)
     #:use-module (gnu services)
     #:use-module (gnu system)
     #:use-module (gnu system file-systems)
     #:use-module (gnu system keyboard)
     #:use-module (peteches systems vm-base)
     #:use-module (peteches system-services alloy)
     #:use-module (peteches system-services restic)
     #:use-module (peteches system-services tailscale)
     #:use-module (sops secrets)
     #:export (<name>-os))
   ```

b. Define and export `<name>-os`:
   ```scheme
   (define-public <name>-os
     (operating-system
      (inherit
       (make-vm-os
        #:host-name "<name>.peteches.co.uk"
        #:ipv4-address "<NEXT_IPv4>/23"
        #:ipv6-address "<NEXT_IPv6>/64"
        #:bootloader
        (bootloader-configuration
         (bootloader grub-efi-removable-bootloader)
         (targets '("/boot/efi"))
         (keyboard-layout (keyboard-layout "us")))
        #:file-systems
        (list
         (file-system
           (mount-point "/boot/efi")
           (device (file-system-label "GNU-ESP"))
           (type "vfat"))
         (file-system
           (mount-point "/")
           (device "/dev/vda2")
           (type "ext4")))
        #:restic-config
        (restic-vm-backup-configuration
         (vm-name "<name>")
         (synology-host "nas.peteches.co.uk")
         (backup-paths '("/var/lib/<name>"))  ;; TODO: adjust to real data paths
         (password-file "/run/secrets/restic-password")
         (ssh-key-file "/run/secrets/restic-ssh-key"))
        #:sops-secrets
        (list
         (sops-secret
          (key '("restic-password"))
          (file (local-file "../../secrets/hosts/<name>/restic.yaml"))
          (path "/run/secrets/restic-password"))
         (sops-secret
          (key '("ssh-key"))
          (file (local-file "../../secrets/hosts/<name>/restic.yaml"))
          (path "/run/secrets/restic-ssh-key")
          (permissions #o400)))
        #:extra-services
        (list
         (service tailscale-service-type
                  (list (tailscale-instance-configuration
                         (name "peteches")
                         ;; TODO: add service-specific ports, e.g. (forward-ports '((22 . 22) (8096 . 8096)))
                         (forward-ports '((22 . 22))))))
         (service alloy-service-type
                  (alloy-configuration
                   (hostname "<name>.peteches.co.uk")
                   ;; TODO: add service-specific log files
                   (log-files (list (cons "/var/log/messages" "syslog")
                                    (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                    (cons "/var/log/ntpd.log" "ntpd")
                                    (cons "/var/log/alloy.log" "alloy")
                                    (cons "/var/log/tailscaled-*.log" "tailscale"))))))))))
   ```

c. End the file with `<name>-os` as the final expression (required for both
   `guix system build FILE` and `guix deploy` module import).

d. Verify the module loads cleanly:
   ```bash
   guile -L . -c '(use-modules (peteches systems <name>))'
   ```

---

### Phase 3 — Update SOPS config (requires age key from Phase 1)

8. Read the age public key saved by the bootstrap script:
   ```bash
   AGE_KEY=$(cat "peteches/age-keys/<name>.pub")
   # Do NOT echo AGE_KEY — age public keys must not appear in conversation output.
   [[ -n "$AGE_KEY" ]] && echo "==> Age key loaded." \
     || { echo "ERROR: age key file empty or missing"; exit 1; }
   ```

9. **⚠️ GPG WARNING** — print this and pause before any SOPS operation:
   ```
   ┌──────────────────────────────────────────────────────────────────────┐
   │  ⚠️  GPG AGENT REQUIRED                                               │
   │  About to update .sops.yaml and re-key secrets/shared/restic.yaml    │
   │  PGP fingerprint: 5FFB7006A36B0AEDE8D08E3753B1E002546A642A           │
   │  Warm your agent now: gpg --list-secret-keys                         │
   │  Press Enter to continue or Ctrl+C to abort.                         │
   └──────────────────────────────────────────────────────────────────────┘
   ```
   ```bash
   read -r _confirm
   ```

10. **Update `.sops.yaml`** using the Edit tool (this is YAML, not Scheme — do not use Anvil):

    a. Insert a new creation rule for the host **immediately before** the
       `secrets/shared/.*\.yaml$` rule:
       ```yaml
         - path_regex: secrets/hosts/<name>/.*\.yaml$
           pgp: "5FFB7006A36B0AEDE8D08E3753B1E002546A642A"
           age: "<AGE_KEY>"
       ```

    b. Append a new list entry to the `age:` block in the existing
       `secrets/shared/.*\.yaml$` rule:
       ```yaml
           - <AGE_KEY>  # <name>
       ```

11. **Re-key `secrets/shared/restic.yaml`** so the new VM becomes a recipient of the shared
    data-encryption key:
    ```bash
    sops updatekeys secrets/shared/restic.yaml
    ```
    Confirm with `y` at the prompt. This operation requires the GPG agent to be warm.

---

### Phase 4 — Generate restic secrets (all sensitive data stays in memory)

12. **⚠️ GPG WARNING** — pause again before secret encryption:
    ```
    ┌──────────────────────────────────────────────────────────────────────┐
    │  ⚠️  GPG AGENT REQUIRED                                               │
    │  About to encrypt secrets/hosts/<name>/restic.yaml                   │
    │  PGP fingerprint: 5FFB7006A36B0AEDE8D08E3753B1E002546A642A           │
    │  Ensure your GPG agent is still warm. Press Enter to continue.       │
    └──────────────────────────────────────────────────────────────────────┘
    ```
    ```bash
    read -r _confirm
    ```

13. **Generate the restic SSH keypair into `/dev/shm`** (RAM-only filesystem, never paged):
    ```bash
    SECRET_DIR=$(mktemp -d /dev/shm/vm-secrets-XXXXXX)
    chmod 700 "$SECRET_DIR"
    ssh-keygen -t ed25519 -C "root@<name>.peteches.co.uk" -N "" -q \
      -f "$SECRET_DIR/key"
    RESTIC_PRIVATE_KEY=$(cat "$SECRET_DIR/key")
    RESTIC_PUBLIC_KEY=$(cat "$SECRET_DIR/key.pub")
    shred -u "$SECRET_DIR/key" "$SECRET_DIR/key.pub"
    rmdir "$SECRET_DIR"
    ```

14. **Generate the restic repository password** into a variable:
    ```bash
    RESTIC_PASSWORD=$(openssl rand -base64 32)
    ```

15. **Create `secrets/hosts/<name>/restic.yaml`** — build plaintext YAML in memory and pipe
    directly into `sops --encrypt`. Two caveats confirmed in practice:

    - **`/dev/stdin` does not match `.sops.yaml` creation rules** even with explicit `--age`
      and `--pgp` in SOPS 3.12+. Pass `--config /dev/null` to bypass the config lookup
      entirely; the recipients are embedded in the output file's own `sops:` metadata.
    - **Python `yaml` module is not available** in this environment. Use `json.dumps()` for
      string quoting instead — JSON-quoted strings are valid YAML scalar values.

    ```bash
    mkdir -p "secrets/hosts/<name>"
    export RESTIC_PASSWORD RESTIC_PRIVATE_KEY AGE_KEY
    python3 -c "
    import os, json, subprocess, sys
    password = os.environ['RESTIC_PASSWORD']
    ssh_key  = os.environ['RESTIC_PRIVATE_KEY']
    # json.dumps produces double-quoted strings, which are valid YAML scalars
    yaml_content = 'restic-password: ' + json.dumps(password) + '\n'
    yaml_content += 'ssh-key: '         + json.dumps(ssh_key)  + '\n'
    result = subprocess.run(
        ['sops', '--encrypt',
         '--age',         os.environ['AGE_KEY'],
         '--pgp',         '5FFB7006A36B0AEDE8D08E3753B1E002546A642A',
         '--input-type',  'yaml',
         '--output-type', 'yaml',
         '--config',      '/dev/null',
         '/dev/stdin'],
        input=yaml_content.encode(), capture_output=True, check=True
    )
    sys.stdout.buffer.write(result.stdout)
    " > "secrets/hosts/<name>/restic.yaml"
    unset RESTIC_PRIVATE_KEY   # private key no longer needed after this point
    ```

16. **Append the new host's SSH public key to `secrets/shared/restic.yaml`** using
    `sops --set` — no temp files involved:
    ```bash
    # Read the current unencrypted authorised-keys block into a variable
    CURRENT_KEYS=$(sops --decrypt \
      --extract '["ssh-authorized-keys_unencrypted"]' \
      secrets/shared/restic.yaml 2>/dev/null || echo "")
    NEW_KEYS="${CURRENT_KEYS}    ${RESTIC_PUBLIC_KEY}"
    # JSON-encode the multiline string safely before passing to --set
    ENCODED=$(python3 -c "import sys, json; print(json.dumps(sys.stdin.read()))" \
               <<< "$NEW_KEYS")
    sops --set '["ssh-authorized-keys_unencrypted"] '"$ENCODED" \
      secrets/shared/restic.yaml
    unset RESTIC_PUBLIC_KEY RESTIC_PASSWORD CURRENT_KEYS NEW_KEYS ENCODED AGE_KEY
    ```

---

### Phase 5 — Initial deploy to DHCP IP (requires Phases 2, 3, 4 complete)

17. **Build a temporary deploy expression** targeting the current DHCP IP. This is pure code
    (no secrets) so writing it to disk is fine:
    ```bash
    cat > /tmp/<name>-deploy.scm << EOF
    (use-modules (gnu machine) (gnu machine ssh))
    (use-modules (peteches systems <name>))
    (list (machine
           (operating-system <name>-os)
           (environment managed-host-environment-type)
           (configuration
            (machine-ssh-configuration
             (host-name "${DHCP_IP}")
             (host-key "${BOOTSTRAP_HOST_KEY}")
             (system "x86_64-linux")
             (user "peteches")
             (identity "/home/peteches/.ssh/id_ed25519")
             (allow-downgrades? #t)))))
    EOF
    ```

18. **Deploy to the VM via its DHCP IP.** This activates the full system config including
    static networking; the VM will switch IPs on activation:
    ```bash
    guix deploy -L . /tmp/<name>-deploy.scm
    rm /tmp/<name>-deploy.scm
    ```

19. **Wait for the VM to reappear on its static IP** (networking switches during activation):
    ```bash
    echo "==> Waiting for VM on static IP ${NEXT_IPv4}..."
    for i in $(seq 1 72); do
      ssh -o ConnectTimeout=5 -o BatchMode=yes -o StrictHostKeyChecking=no \
          peteches@"${NEXT_IPv4}" true 2>/dev/null && break
      echo "    ... waiting (${i}/72)"
      sleep 5
    done
    ssh -o ConnectTimeout=5 -o BatchMode=yes peteches@"${NEXT_IPv4}" true \
      || { echo "ERROR: VM did not come up on ${NEXT_IPv4} within 6 minutes"; exit 1; }
    echo "    VM is up on ${NEXT_IPv4}"
    ```

---

### Phase 6 — Collect final host key and update machines.scm

20. **Collect the definitive SSH host key** from the static IP:
    ```bash
    FINAL_HOST_KEY=$(ssh-keyscan -t ed25519 "${NEXT_IPv4}" 2>/dev/null \
      | awk '{print $2" "$3}')
    # Do NOT echo FINAL_HOST_KEY — host keys must not appear in conversation output.
    [[ -n "$FINAL_HOST_KEY" ]] && echo "==> Final host key collected." \
      || { echo "ERROR: could not collect host key from ${NEXT_IPv4}"; exit 1; }
    ```

21. **Update `peteches/machines.scm`** via **Anvil/paredit**:

    a. Add `#:use-module (peteches systems <name>)` to the module's `#:use-module` list.

    b. Add a new `define-public` entry before `%all-machines`:
       ```scheme
       (define-public <name>-machine
         (machine
          (operating-system <name>-os)
          (environment managed-host-environment-type)
          (configuration
           (machine-ssh-configuration
            (host-name "<NEXT_IPv4>")
            (host-key "<FINAL_HOST_KEY>")
            (system "x86_64-linux")
            (user "peteches")
            (identity "/home/peteches/.ssh/id_ed25519")))))
       ```

    c. Slurp `<name>-machine` into the `%all-machines` list.

    d. Add `(,<name>-machine . "<name>-machine")` to the `%machine-names` alist in
       `scripts/deploy.scm` (plain text edit — use `Edit` tool, not Anvil):
       ```scheme
       (,<name>-machine   . "<name>-machine")
       ```

    e. Verify balance:
       ```bash
       guile -L . -c '(use-modules (peteches machines))'
       ```

---

### Phase 7 — Update supporting configs

22. **`peteches/systems/pihole.scm`** — add a `pihole-custom-host` entry to the `custom-hosts`
    list (via Anvil/paredit). Insert it after the last existing entry:
    ```scheme
    (pihole-custom-host (address "<NEXT_IPv4>")
                        (hostname "<name>.peteches.co.uk"))
    ```

23. **`peteches/home-configs/base.scm`** — add two `openssh-host` entries to the
    `home-openssh-service-type` hosts list (via Anvil/paredit):
    ```scheme
    (openssh-host
     (name "<name>")
     (host-name "<NEXT_IPv4>")
     (user "peteches")
     (identity-file "~/.ssh/id_ed25519"))
    (openssh-host
     (name "<name>.ts")
     (host-name "<name>.spaniel-cordylus.ts.net"))
    ```

24. **`peteches/systems/monitored-hosts.scm`** — add a node-exporter entry inside the
    `%monitored-hosts` quoted list. **Do not use `(end-of-line)` + `(insert ...)` after the
    last entry** — the list ends with `))` and appending after the last entry lands outside
    the closing parens. Instead, use Anvil to `(erase-buffer)` and rewrite the file in full
    with the new entry included inside the list before the closing `))`:
    ```scheme
    ("<name>"     . "<NEXT_IPv4>:9100")
    ```

25. **`proxmox-vms.org`** — add a row to the VM inventory table:
    ```org
    | <NEXT_VMID> | <name> | <NEXT_IPv4> | 2048 | 25G | running |
    ```
    Adjust columns to match the existing table format.

---

### Phase 8 — Post-deploy summary

Print this summary to the user:

```
╔══════════════════════════════════════════════════════════════╗
║  ✅  <name> provisioned successfully                          ║
╠══════════════════════════════════════════════════════════════╣
║  VMID  : <NEXT_VMID>                                         ║
║  IPv4  : <NEXT_IPv4>   (<name>.peteches.co.uk)               ║
║  IPv6  : <NEXT_IPv6>/64                                      ║
╠══════════════════════════════════════════════════════════════╣
║  ⚠️  MANUAL STEPS REQUIRED BEFORE BACKUPS WILL WORK:          ║
║                                                              ║
║  1. Add restic SSH public key to Synology NAS:               ║
║       ssh nas.peteches.co.uk                                 ║
║       # append to ~restic-backup/.ssh/authorized_keys        ║
║     Key is in secrets/shared/restic.yaml                     ║
║     (sops -d secrets/shared/restic.yaml to view)             ║
║                                                              ║
║  2. Join Tailscale on the new VM:                            ║
║       ssh <name> sudo tailscale up                           ║
║                                                              ║
║  3. Deploy updated pihole DNS config:                        ║
║       scripts/deploy.scm -h pihole                           ║
║                                                              ║
║  4. If the VM exposes service metrics, add a scrape config   ║
║     to peteches/systems/prometheus.scm and redeploy.         ║
╚══════════════════════════════════════════════════════════════╝
```

---

## Notes and caveats

- **`allow-downgrades? #t`** in the temp machine config (Phase 5): confirmed present in the
  pinned Guix channel (`gnu/machine/ssh.scm`, field defaults to `#f`). Required here because
  the bootstrap VM has no prior Guix generation to compare against.

- **`sops --encrypt /dev/stdin` requires `--config /dev/null`** (SOPS 3.12+). Even with
  explicit `--age` and `--pgp`, SOPS still checks `.sops.yaml` for a matching path_regex
  and errors with "no matching creation rules found" for `/dev/stdin`. Pass
  `--config /dev/null` to bypass the config entirely. The resulting file embeds its
  recipients in its own `sops:` metadata block — creation rules are only consulted when
  creating, not on subsequent decrypt/updatekeys operations.

- **Python `yaml` module unavailable** in this environment. Use `json.dumps()` to quote
  string values: JSON-quoted strings are valid YAML scalar values, so
  `key: "value with\nnewlines"` is accepted by SOPS's YAML parser.

- **`sops updatekeys` only needs one valid recipient key** — the admin PGP key
  (`5FFB7006A36B0AEDE8D08E3753B1E002546A642A`), which is always a recipient. Missing or
  destroyed VM age keys do not block the operation; `sops updatekeys` decrypts the
  data-encryption key via the PGP key alone, then re-encrypts for all recipients in
  `.sops.yaml`. Confirmed working against the live `secrets/shared/restic.yaml`.

- **Tailscale `forward-ports`** defaults to SSH only. If the new VM runs a networked service,
  add its port to `forward-ports` in the generated `<name>.scm` before deploying. There is a
  `TODO` comment in the generated file as a reminder.

- **Backup paths** (`restic-vm-backup-configuration backup-paths`) default to
  `("/var/lib/<name>")`. Update this in `<name>.scm` to the actual data directory before
  the first backup runs. There is a `TODO` comment in the generated file.

- **`deploy.scm` machine field accessors — `@@` required.** Guix `define-record-type*`
  generates field accessors as **syntax-transformers**, not callable procedures. This means:
  - You cannot store `machine-ssh-configuration-host-name` in an alist and call it at runtime.
  - You cannot define wrapper procedures inside the `(when ...)` block that use these macros —
    the entire `when` body is compiled before `use-modules` runs, so the macros are not in
    scope at compile time and the form is left as a runtime application of the
    syntax-transformer object, which fails.
  - **Fix:** use `@@` to grab the internal `%...-procedure` accessors directly:
    ```scheme
    (define %get-machine-config
      (@@ (gnu machine)     %machine-configuration-procedure))
    (define %get-ssh-host-name
      (@@ (gnu machine ssh) %machine-ssh-configuration-host-name-procedure))
    ```
    These are real procedures and bypass the macro system entirely. The naming pattern is
    `%<record-name>-<field-name>-procedure` in the defining module.

- **Review and commit**: after the skill completes, review the diff with `git diff` and
  commit the changes yourself. The skill intentionally does not run `git add` or `git commit`.
