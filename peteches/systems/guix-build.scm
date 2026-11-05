;; guix-build.scm — Guix substitute server + build-offload target, on a
;; Proxmox QEMU/KVM VM.
;;
;; Successor to nug's guix-publish/offload role — nug itself was reinstalled
;; as the Proxmox host (proxmox3), so this can no longer live there.
;; %nug-build-machine in (peteches systems common) is repointed at this VM's
;; hostname/host-key (kept that name for minimal diff churn -- see that
;; module's comment), and the desktops' `without-gdm` substitute-server URL
;; and authorized-keys (base.scm) point here too, using
;; guix-build-substitute-key.pub (this VM's /etc/guix/signing-key.pub,
;; fetched post-boot). Still outstanding: dagon (nug's successor desktop)
;; has no offload keypair yet, so it isn't in this VM's
;; guix-offload-authorized-keys below.
;;
;; The guix-publish host below MUST be "0.0.0.0", not "::": the fleet is
;; IPv4-only (Tailscale DNS serves no AAAA records for .ts.net names) and
;; shepherd binds every AF_INET6 endpoint with IPV6_V6ONLY=1, so "::"
;; would be unreachable from any fleet client ("Connection refused" on
;; port 3000). The service type supports only a single endpoint, so
;; dual-stack is not possible. The coordinator-signing-key trust and the
;; per-VM guix-offload authorized-keys list below are carried over from
;; nug.scm's setup.

(define-module (peteches systems guix-build)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services ssh)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches services alloy)
  #:use-module (peteches services firewall)
  #:use-module (peteches services restic)
  #:use-module (peteches services tailscale)
  #:use-module (sops secrets)
  #:export (guix-build-os))

(define-public guix-build-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "guix-build.peteches.co.uk"
     #:ipv4-address "192.168.51.207/23"
     #:ipv6-address "2a10:d582:ef59::113/64"
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
     #:users-extra
     (list (user-account
            (name "guix-offload")
            (comment "Build offload user")
            (group "users")
            (system? #t)
            (home-directory "/var/empty")))
     ;; restic-config deliberately omitted for now: it needs the fleet's
     ;; shared restic SSH key (the one nas.peteches.co.uk already trusts),
     ;; not a freshly generated one -- reusing the real credential requires
     ;; verifying it against an existing host's secret first rather than
     ;; guessing. Revisit once this VM exists and that can be confirmed.
     #:sops-secrets '()
     ;; This VM IS the offload target now, not a client of one.
     #:with-nug-offload? #f
     #:extra-services
     (list
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches"))))
      (simple-service 'guix-publish-firewall
                      firewall-service-type
                      (nftables-rules
                       (input (list "tcp dport 3000 accept comment \"guix-publish\""))))
      (service guix-publish-service-type
               (guix-publish-configuration
                (host "0.0.0.0")
                (port 3000)
                (compression '(("zstd" 9)))
                ;; #f, not nug.scm's #t: advertise? needs avahi-daemon for
                ;; mDNS, which nug had as a desktop service but this headless
                ;; VM (vm-base.scm) does not run. Nothing in this fleet
                ;; discovers the substitute server via mDNS anyway -- every
                ;; consumer hardcodes guix-build.spaniel-cordylus.ts.net:3000
                ;; (see common.scm's %authorize-coordinator-key).
                (advertise? #f)
                (cache "/var/cache/guix/publish")))
      ;; Every box that offloads builds TO this VM pushes its input closure
      ;; here first, signed by its own /etc/guix/signing-key.pub -- so this
      ;; VM's daemon has to authorize each one or the connection succeeds,
      ;; the build request is accepted, and the archive send is refused with
      ;; "unauthorized public key", which guix reports as a plain build
      ;; failure and quietly falls back to building locally. That fallback is
      ;; what kept this list half-wired for so long: offload looked like it
      ;; was working (it connected, ssh auth passed) and only the archive
      ;; step failed. Until 2026-11-05 it held exactly ONE key
      ;; (claude-workstation's), and even that one had gone stale when that
      ;; VM was rebuilt from the MinIO image on 2026-10-27 -- offload from
      ;; claude-workstation has been falling back to local builds since.
      ;;
      ;; Populated 2026-11-05 from each box's live /etc/guix/signing-key.pub,
      ;; fetched over ssh against machines.scm's pinned host keys (so the key
      ;; came from a host-key-verified peer, not from a copy) and then
      ;; hash-checked entry by entry against the same file on the box. Keys
      ;; here are compared as the set of #...# fingerprint tokens they are
      ;; built from, not as whole-file hashes:
      ;;   grep -o '#[0-9A-Fa-f]*#' <file> | sha256sum | cut -c1-10
      ;;
      ;; NOT here, and why: rustdesk and nyarlothotep have an offload keypair
      ;; (secrets/hosts/<them>/guix-build.yaml) and are already in the ssh
      ;; list below, but neither box was reachable on 2026-11-05, so there was
      ;; no verified signing key to put here -- their offload keeps falling
      ;; back to local builds until it is fetched and added. nyarlothotep is
      ;; the asymmetry to watch for: its ssh half is in the list below and its
      ;; archive half is not, and the two trusts have to be kept in step by
      ;; hand. vault, plane, comfyui, dagon and the critical-grind/outlines
      ;; have no offload keypair at all, so offload from them cannot even
      ;; authenticate as guix-offload; adding their signing keys here would be
      ;; decoration, not wiring.
      (simple-service 'guix-offload-signing-keys
                      guix-service-type
                      (guix-extension
                       (authorized-keys
                        (list
                        (plain-file "agents-signing-key.pub"
                                          "(public-key \n (ecc \n  (curve Ed25519)\n  (q #F4004CE8FDDF38E595E3E3F729AAA8A859A9668426D9A97625D9BB06F3E92505#)\n  )\n )\n")
                        (plain-file "arr-signing-key.pub"
                                          "(public-key \n (ecc \n  (curve Ed25519)\n  (q #F8790D830443BE34C9BFA0094C4EBF53B05DAAB7D9CC1EED68C27A6F1864B53A#)\n  )\n )\n")
                        (plain-file "caddy-signing-key.pub"
                                          "(public-key \n (ecc \n  (curve Ed25519)\n  (q #2F0EDD2DCB06E68EA337FCBED653CEF897EB57A6ED1E09DD949A04A0752B5761#)\n  )\n )\n")
                        (plain-file "claude-workstation-signing-key.pub"
                                          "(public-key \n (ecc \n  (curve Ed25519)\n  (q #1A6D33B325CD6BA778237B44FFD7DB8A52388CEB4C721F02AC2B4C7A388A3E3D#)\n  )\n )\n")
                        (plain-file "concourse-db-signing-key.pub"
                                          "(public-key \n (ecc \n  (curve Ed25519)\n  (q #72F26FF2FEF90828672D1C9D5042F45C1131706AB43D0C78A1E3D9B1C1CB0CFE#)\n  )\n )\n")
                        (plain-file "concourse-web01-signing-key.pub"
                                          "(public-key \n (ecc \n  (curve Ed25519)\n  (q #AB41D70ABEEEAB9ED42421D544225FF14497D4F12726618EBB888887FC6D5F30#)\n  )\n )\n")
                        (plain-file "concourse-worker01-signing-key.pub"
                                          "(public-key \n (ecc \n  (curve Ed25519)\n  (q #5BE3C868E7B0C0D78007155B278EC33EB103CF78DACD43252D6F3AB09BBA65B2#)\n  )\n )\n")
                        (plain-file "downloads-signing-key.pub"
                                          "(public-key \n (ecc \n  (curve Ed25519)\n  (q #B612C4C2ACA4C3CBF3AEB3DF6DABB43848DEC7055CB70AE872D21280C8846FF7#)\n  )\n )\n")
                        (plain-file "git-signing-key.pub"
                                          "(public-key \n (ecc \n  (curve Ed25519)\n  (q #64F15C0142989CE13EBAE661933E8255E9D2B01C5679415E67D275C808796104#)\n  )\n )\n")
                        (plain-file "grafana-signing-key.pub"
                                          "(public-key \n (ecc \n  (curve Ed25519)\n  (q #7E2059149175B499A495DCD04A9C55417EC73535263BF461A8457D721CECA6B1#)\n  )\n )\n")
                        (plain-file "jellyfin-signing-key.pub"
                                          "(public-key \n (ecc \n  (curve Ed25519)\n  (q #D76B2155EB6DACF6DA52766ECB3B2B8E140B288ACABF479E7692A30FE1F1251C#)\n  )\n )\n")
                        (plain-file "loki-signing-key.pub"
                                          "(public-key \n (ecc \n  (curve Ed25519)\n  (q #C45A1F7B839D2FEADE560338156B11D15D92EE5FCF100610DCEE5404E2C47AB8#)\n  )\n )\n")
                        (plain-file "pihole-signing-key.pub"
                                          "(public-key \n (ecc \n  (curve Ed25519)\n  (q #32FD8219F7BFE7AAAF40832F81F539CDA7260BDBC87A0546E358EEB53173BEE5#)\n  )\n )\n")
                        (plain-file "prometheus-signing-key.pub"
                                          "(public-key \n (ecc \n  (curve Ed25519)\n  (q #D53183A990CDBFBEE87323948146E93CFC8DAF3D6DF3B7EDAC177F8568380D6F#)\n  )\n )\n")
                        (plain-file "prowlarr-signing-key.pub"
                                          "(public-key \n (ecc \n  (curve Ed25519)\n  (q #662D86C273C94C01D3AD544210C6FDEBCCF192B12D7A006F984EC2050ED039A3#)\n  )\n )\n")
))))

      (simple-service 'guix-offload-authorized-keys
                      openssh-service-type
                      `(("guix-offload"
                         ,(plain-file "claude-workstation-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIVVAw63bjPSR1Pmm8MixkVZgNYty3IrWbJMyWe7CEVo guix-offload@claude-workstation\n")
                         ,(plain-file "arr-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJCmWvNZoNQhpVpeYU6VXtYcrtS8XfgrK5S5WCs5OtM1 guix-offload@arr\n")
                         ,(plain-file "nyarlothotep.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEP+/uHdoUNfL+LuniZGTEwPJkxvSgDpuR58yxfw/u74 guix-build@nyarlothotep\n")
                         ,(plain-file "caddy-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ5OyvcOFlI3lnunv9FzkOms2CO9i7y12EnSSBDmp6ob guix-offload@caddy\n")
                         ,(plain-file "downloads-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOTVFFGPzZsX0hV4fY2bhptvW1Zs6lilcYMGTOli1UoL guix-offload@downloads\n")
                         ,(plain-file "git-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDP/krMF4ECdoMVIqv9K5mZHvbJUv7+ZFSx2FlVlHSOf guix-offload@git\n")
                         ,(plain-file "grafana-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINw3+hJHHSzwhGquTWRtXx5+uVdvarpu3gJGCnXrj61Q guix-offload@grafana\n")
                         ,(plain-file "jellyfin-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILGGBMFH7Bei/lTu2s4xqFveXOmxOdqHGQiVmnDRfBt5 guix-offload@jellyfin\n")
                         ,(plain-file "loki-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN9R8rSJi1VRa2okQhXFxxBHJXwmV1rVOl8HelpepFVg guix-offload@loki\n")
                         ,(plain-file "pihole-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMNAir7xhAl7Z50tloQKOfCeVPqTqDgmIuSVxtfFdLES guix-offload@pihole\n")
                         ,(plain-file "prometheus-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM+nhIk+ySFYMj7I4SDwA/LKyM8MH3+8NMIabyAIuMSC guix-offload@prometheus\n")
                         ,(plain-file "prowlarr-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPxWQLDvXJGgp4HsOpSEMyLTGi0lL2zYcvRvARuVv/nU guix-offload@prowlarr\n")
                         ,(plain-file "rustdesk-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII0DTgAJjaG1+0STwTBDRfUrbP/q0KFVnY5OdjrqKasS guix-offload@rustdesk\n")
                         ,(plain-file "agents-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAqCZu5kUl0/ghyS01gQJDrRWhn+ZnVJ/ofqaP/L2dd6 guix-offload@agents\n")
                         ,(plain-file "concourse-db-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFSFUzB2qQ/oYwfwYk2rfNvnLwoSmjWYvqZaMuH3saCq concourse-db-guix-build\n")
                         ,(plain-file "concourse-web01-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIObAHuUSuAvA27OHWQlCp6MbSpaK9usd6gMA1YYzj5Kx concourse-web01-guix-build\n")
                         ,(plain-file "concourse-worker01-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEP76t+Fz1kBP8RpBJ5iDlJKZvN4NjuSFeJoR31nu/xw concourse-worker01-guix-build\n")
                         ;; New since this list was on nug: comfyui itself
                         ;; offloads TO this VM (with-nug-offload? default #t
                         ;; in comfyui.scm), so its key needs to land here too
                         ;; once generated -- but it has NO offload keypair yet
                         ;; (secrets/hosts/comfyui/guix-build.yaml does not
                         ;; exist), so nothing can be added for it until one is
                         ;; created; dagon is in the same position. The four
                         ;; entries above this comment (agents + the three
                         ;; concourse boxes) were added 2026-11-05 from each
                         ;; box's live /run/secrets/guix-offload-key, verified
                         ;; against the same file on the box.
                         )))
      (service alloy-service-type
               (alloy-configuration
                (hostname "guix-build.peteches.co.uk")
                (log-files (list (cons "/var/log/messages" "syslog")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/ntpd.log" "ntpd")
                                 (cons "/var/log/alloy.log" "alloy")
                                 (cons "/var/log/tailscaled-*.log" "tailscale"))))))))))

guix-build-os
