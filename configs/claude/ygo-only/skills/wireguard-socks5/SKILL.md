---
name: wireguard-socks5
description: "Use when the user on claude-workstation's ygo account wants to reach something over the WireGuard tunnel, mentions the VPN/tunnel/SOCKS5 proxy, or asks to route a specific request/tool through it. This host has a split-tunnel WireGuard interface (wg0) that never becomes the default route -- only traffic explicitly sent through the local SOCKS5 proxy at 127.0.0.1:1080 goes over it."
---

# WireGuard split-tunnel + SOCKS5 proxy (claude-workstation, ygo only)

This VM (`claude-workstation`) has a WireGuard tunnel (`wg0`) whose only
purpose is to feed a local SOCKS5 proxy at `127.0.0.1:1080`. It is a
**split tunnel**: the system's default route is never touched. General
internet traffic — including your own calls back to Anthropic — is
completely unaffected whether the tunnel is up or not.

The netns, tunnel and proxy now auto-start on boot (`auto-start? #t` in
`peteches/systems/claude-workstation.scm`'s `wireguard-socks5-service-type`
instance), so normally nothing needs starting by hand — check status with
`sudo herd status wireguard-wg0` / `socks5-proxy` first. Only use the
commands below after a manual stop, or if status shows either service down.

Only the `ygo` account's Claude Code knows about this skill; it is not
shipped to the `peteches` or `criticalgrind` accounts on the same VM.

## Bringing the tunnel up (if it isn't already)

```bash
sudo herd start wireguard-wg0
sudo herd start socks5-proxy
```

## Using it

Once both services report running, point whatever needs the tunnel at the
SOCKS5 proxy rather than assuming traffic is routed automatically — nothing
else on the box goes through `wg0`:

```bash
curl --socks5-hostname 127.0.0.1:1080 https://example.com
# or for the whole shell session:
export ALL_PROXY=socks5h://127.0.0.1:1080
```

Git, npm, and most other CLIs accept an equivalent `--proxy` /
`https-proxy` / `http.proxy` setting pointed at the same
`socks5h://127.0.0.1:1080` address.

## Tearing it down

Stop the proxy before the tunnel:

```bash
sudo herd stop socks5-proxy
sudo herd stop wireguard-wg0
```

## What NOT to do

- Don't try to read, decrypt, or print
  `secrets/hosts/claude-workstation/wireguard.yaml` (SOPS-encrypted) or
  anything under `/run/secrets/wg0.conf` — that's the tunnel's private key,
  peer identity, and endpoint. Treat it exactly like any other secret file
  per the global secret-handling rules.
- Don't assume a service or command routes through the tunnel just because
  `wg0` is up — only processes that explicitly dial the SOCKS5 proxy do
  (this is enforced by an fwmark/policy-routing rule tied to the dedicated
  `socks5` system user, not by anything client-side).

See `docs/secrets-management.org` ("WireGuard split-tunnel secret") and
`peteches/services/wireguard-socks5.scm` in the guix repo for the full
implementation if more detail is ever needed.
