---
name: caddy-new-vhost
description: "Use when adding a new Caddy reverse proxy vhost (e.g. *-ts.peteches.co.uk) that requires a deSEC CNAME record and Let's Encrypt TLS certificate — this process is new and different from the existing vhosts that were added during initial caddy VM provisioning."
---

# Adding a new Caddy reverse proxy vhost with Let's Encrypt

This skill covers the full process for adding a new `*.ts.peteches.co.uk` Caddy
reverse proxy route that requires:

1. A reverse proxy entry in `peteches/systems/caddy.scm`
2. A deSEC CNAME record pointing at `caddy.spaniel-cordylus.ts.net`
3. Let's Encrypt TLS certificate acquisition via deSEC DNS-01 challenge
4. Monitoring Caddy logs for successful certificate issuance

This is the pattern used when `rerank.ts.peteches.co.uk` was added
(2025-11-28). The critical difference from the initial caddy provisioning is
that the CNAME record must be created **manually via the deSEC API** before
Caddy can obtain a certificate — the ACME DNS-01 challenge fails with
`NXDOMAIN looking up TXT for _acme-challenge.<new-host>.ts.peteches.co.uk`
until the CNAME is visible in DNS.

## Prerequisites

- The target upstream service exists and is reachable at the target host:port
- You have the caddy VM's SSH access (`peteches@caddy.spaniel-cordylus.ts.net`)
- You know the caddy VM's deSEC token (stored at `/run/secrets/desec-token` on
  the VM, encrypted by SOPS)

## Step 1: Add the Caddy reverse proxy entry

Edit `peteches/systems/caddy.scm`, adding a `(caddy-reverse-proxy ...)` entry
to the `virtual-hosts` list, following the existing convention:

```scheme
;; <description> of the target service.
(caddy-reverse-proxy
 (domain "<new-host>.ts.peteches.co.uk")
 (upstream "<upstream-host>.spaniel-cordylus.ts.net:<port>"))
```

Key patterns from existing entries:
- If the upstream speaks plain HTTP and the tailnet already encrypts the
  caddy↔upstream hop, no TLS backend is needed (no `tls-backend?`).
- If the upstream requires TLS but self-signed, use `(tls-backend? #t)`
  with `(tls-insecure-skip-verify? #t)`.
- Always add a comment block explaining what the upstream service is and
  why this vhost exists — future maintainers will thank you.

## Step 2: Validate and commit

```bash
# Parse check (catches unbalanced parens quickly)
guile -c '(call-with-input-file "peteches/systems/caddy.scm"
            (lambda (p) (let loop () (unless (eof-object? (read p)) (loop)))))'

# Load-check (fresh process per module)
guix repl -L . -L "$CHANNEL" -- /dev/stdin <<EOF >/dev/null 2>&1 && echo OK || echo FAIL
(resolve-interface '(peteches systems caddy))
EOF
```

If the parse check passes, it's safe to commit:

```bash
git add peteches/systems/caddy.scm
git commit -m "caddy: add <new-host>.ts.peteches.co.uk route to <upstream>"
```

## Step 3: Deploy to the caddy VM

```bash
./scripts/deploy.scm -h caddy
```

This rebuilds and switches the caddy system generation. The deployment
script handles the CNAME bootstrap (`caddy-ensure-cnames` shepherd service),
but **this only runs for vhosts declared in the current config at deploy
time**. If the CNAME record was missing at deploy time (which it will be for
a new vhost), the one-shot service won't create it. You need to do it by hand.

## Step 4: Restart caddy and monitor logs

After a successful deploy, restart the caddy service and follow its logs:

```bash
ssh peteches@caddy.spaniel-cordylus.ts.net \
  'sudo herd restart caddy && sudo tail -f /var/log/caddy.log'
```

Watch for these log lines in order:

```
{"level":"info",...,"msg":"acquiring lock","identifier":"<new-host>.ts.peteches.co.uk"}
{"level":"info",...,"msg":"lock acquired","identifier":"<new-host>.ts.peteches.co.uk"}
{"level":"info",...,"msg":"obtaining certificate","identifier":"<new-host>.ts.peteches.co.uk"}
{"level":"info",...,"msg":"using ACME account","account_id":"..."}
{"level":"info",...,"msg":"trying to solve challenge",...,"challenge_type":"dns-01"}
```

If the CNAME is missing, you'll see:

```
{"level":"error",...,"msg":"challenge failed",...,"problem":{
  "type":"urn:ietf:params:acme:error:dns",
  "detail":"DNS problem: NXDOMAIN looking up TXT for _acme-challenge.<new-host>.ts.peteches.co.uk"}}
```

This means Step 5 is needed.

## Step 5: Create the deSEC CNAME record

Get the deSEC token from the caddy VM:

```bash
ssh peteches@caddy.spaniel-cordylus.ts.net \
  'sudo cat /run/secrets/desec-token'
```

Then create the CNAME via the deSEC API. The subname is the part before
`.ts.peteches.co.uk`, and the record points to `caddy.spaniel-cordylus.ts.net.`
(note the trailing dot):

```bash
curl --cacert /etc/ssl/certs/ca-certificates.crt \
  --header 'Authorization: Token <TOKEN>' \
  --header 'Content-Type: application/json' \
  --request PATCH \
  --data '[{"subname":"<new-host>","type":"CNAME","ttl":3600,"records":["caddy.spaniel-cordylus.ts.net."]}]' \
  https://desec.io/api/v1/domains/ts.peteches.co.uk/rrsets/
```

After creating the CNAME, Caddy will re-attempt the ACME challenge on the
next retry cycle (typically within 60s). Back in your `tail -f` window, watch
for:

```
{"level":"info",...,"msg":"authorization finalized","authz_status":"valid"}
{"level":"info",...,"msg":"validations succeeded; finalizing order"}
{"level":"info",...,"msg":"successfully downloaded available certificate chains"}
{"level":"info",...,"msg":"certificate obtained successfully","identifier":"<new-host>.ts.peteches.co.uk"}
```

This confirms production Let's Encrypt succeeded. Note that Caddy may try
the staging CA first (you'll see `acme-staging-v02.api.letsencrypt.org`),
but it falls back to the production CA automatically. Both are fine for
testing, but you want the production certificate — the final log line above
confirms it's from `acme-v02.api.letsencrypt.org-directory` (no "staging").

## Step 6: Verify the endpoint

```bash
curl --cacert /etc/ssl/certs/ca-certificates.crt -sI \
  https://<new-host>.ts.peteches.co.uk/
```

A 200 or at least a non-timeout response confirms the vhost resolves, TLS
serves, and the upstream is reachable. The status code depends on the upstream
(GETting a root path on some APIs returns 400 or 405 — that's fine, the TLS
handshake and proxy connectivity are what matter).

## Step 7: Update the upstream service's comment

Update the comment block in the upstream VM's system config (e.g.
`peteches/systems/comfyui.scm`) to reflect that a Caddy route now exists.
Replace the "There is no caddy route for this endpoint" paragraph with a
note about the new vhost.

## Common pitfalls

1. **CNAME was never created.** The `caddy-ensure-cnames` one-shot service
   runs during deploy and creates CNAME records for **existing** vhosts. A
   newly-added vhost (one you just added in the same commit that deploys)
   was not present when the service ran, so the CNAME must be created by
   hand via the deSEC API. This is how `embeddings.ts` also failed to
   auto-create — documented in the `comfyui.scm` header.

2. **DNS propagation lag.** After creating the CNAME, wait ~15-30 seconds
   before checking logs. The deSEC CNAME must be visible to Let's Encrypt's
   secondary validation before the TXT challenge will pass.

3. **Staging vs production.** Caddy will attempt staging first (rate limits
   are higher, certificate is not trusted by browsers). Once the staging
   cert is obtained, it retries with the production CA. You want the
   production result. Check the issuer field —
   `acme-v02.api.letsencrypt.org-directory` means production;
   `acme-staging-v02.api.letsencrypt.org-directory` means staging only.

4. **`*.ts.peteches.co.uk` in `tls-subjects`.** The Caddy config already
   covers `tls-subjects '("*.ts.peteches.co.uk")`, so any new `*.ts`
   vhost automatically falls within the existing TLS automation policy —
   no config changes needed on that front.

## When NOT to use this skill

- Adding routes for existing vhosts that already have CNAME records: just
  edit the `virtual-hosts` list and deploy. The CNAME already exists.
- Adding new upstream services that don't need Caddy (e.g. internal API
  calls over tailnet without external TLS termination).
