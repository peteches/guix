# Zone VLANs for the UCG migration. Self-documenting scheme: the VLAN ID is
# the third octet of the subnet (10.0.<vlan>.0/24). The default network
# (VLAN 1, 192.168.50.0/23) is intentionally left untouched — never delete it.
#
# Only `house` runs a DHCP server; every other zone is static (VMs via Guix,
# plus the NAS and Proxmox node).
#
# Every block pins `setting_preference` and `ipv6_interface_type` to valid
# values. Provider v0.59.0 serialises an unset value for either as "" and the
# UCGF (UniFi Network 10.6) rejects the networkconf create with
# api.err.InvalidPayload — the provider's own issues #544/#550/#570 were
# measured against a UCG-Fiber on exactly this firmware. The proper
# omit-when-unset fix (#570) is not in any released provider version yet, so
# we pin the fields instead.

# mgmt — VLAN 10 — Proxmox node (static)
resource "unifi_network" "mgmt" {
  name   = "mgmt"
  subnet = "10.0.10.1/24"
  vlan   = 10
  setting_preference  = "manual"
  ipv6_interface_type = "none"
  dhcp_server = {
    enabled = false
  }
}

# infra — VLAN 20 — pihole, caddy, rustdesk, git, vault, guix-build, concourse (static)
resource "unifi_network" "infra" {
  name   = "infra"
  subnet = "10.0.20.1/24"
  vlan   = 20
  setting_preference  = "manual"
  ipv6_interface_type = "none"
  dhcp_server = {
    enabled = false
  }
}

# monitoring — VLAN 30 — prometheus, grafana, loki (static)
resource "unifi_network" "monitoring" {
  name   = "monitoring"
  subnet = "10.0.30.1/24"
  vlan   = 30
  setting_preference  = "manual"
  ipv6_interface_type = "none"
  dhcp_server = {
    enabled = false
  }
}

# media — VLAN 40 — jellyfin+seerr, prowlarr, arr, downloads (static)
resource "unifi_network" "media" {
  name   = "media"
  subnet = "10.0.40.1/24"
  vlan   = 40
  setting_preference  = "manual"
  ipv6_interface_type = "none"
  dhcp_server = {
    enabled = false
  }
}

# critical-grind — VLAN 50 — critical-grind-campaign, critical-grind-outline, plane (static)
resource "unifi_network" "critical_grind" {
  name   = "critical-grind"
  subnet = "10.0.50.1/24"
  vlan   = 50
  setting_preference  = "manual"
  ipv6_interface_type = "none"
  dhcp_server = {
    enabled = false
  }
}

# apps — VLAN 60 — claude-workstation, comfyui (static)
resource "unifi_network" "apps" {
  name   = "apps"
  subnet = "10.0.60.1/24"
  vlan   = 60
  setting_preference  = "manual"
  ipv6_interface_type = "none"
  dhcp_server = {
    enabled = false
  }
}

# workstations — VLAN 70 — dagon (static)
resource "unifi_network" "workstations" {
  name   = "workstations"
  subnet = "10.0.70.1/24"
  vlan   = 70
  setting_preference  = "manual"
  ipv6_interface_type = "none"
  dhcp_server = {
    enabled = false
  }
}

# house — VLAN 80 — all Wi-Fi, guests, nyarlothotep (the only DHCP zone)
resource "unifi_network" "house" {
  name   = "house"
  subnet = "10.0.80.1/24"
  vlan   = 80
  setting_preference  = "manual"
  ipv6_interface_type = "none"
  dhcp_server = {
    enabled = true
    start   = "10.0.80.10"
    stop    = "10.0.80.254"
    # DNS: point at the Pi-hole once its 10.0.20.x address is assigned.
  }
}

# storage — VLAN 90 — NAS (static)
resource "unifi_network" "storage" {
  name   = "storage"
  subnet = "10.0.90.1/24"
  vlan   = 90
  setting_preference  = "manual"
  ipv6_interface_type = "none"
  dhcp_server = {
    enabled = false
  }
}
