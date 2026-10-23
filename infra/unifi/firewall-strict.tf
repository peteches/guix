# Zone-based firewall: strict inter-VLAN ALLOWs -- part 1 (baseline +
# workstations). Default-deny between zones and intra-zone is provided by
# UniFi's auto Block All catch-all (verified on this UCGF), so only the
# required ALLOWs are authored. These are zone+port scoped for now because the
# 10.0.x.y host addresses are not yet assigned; tighten each to the specific
# IP after migration (see the per-rule comments).

# Zones that run node-exporter (9100) / alloy (12345) and are Prometheus
# scrape targets. Excludes house (Wi-Fi, no node-exporter) and monitoring
# itself (covered by the intra-zone ALLOW below).
locals {
  scrape_targets = {
    mgmt           = unifi_network.mgmt
    infra          = unifi_network.infra
    media          = unifi_network.media
    critical-grind = unifi_network.critical_grind
    apps           = unifi_network.apps
    workstations   = unifi_network.workstations
    storage        = unifi_network.storage
  }
}

# Intra-zone: allow all traffic within each custom zone (the locked ruleset's
# "intra-VLAN traffic allowed by default"). Required because UniFi defaults
# intra-zone traffic in a custom zone to Block All.
resource "unifi_firewall_policy" "intra_zone" {
  for_each = local.zone_networks
  name     = "Intra-zone allow: ${each.key}"
  action   = "ALLOW"
  protocol = "all"

  source = {
    zone_id         = unifi_firewall_zone.zones[each.key].id
    matching_target = "ANY"
  }
  destination = {
    zone_id         = unifi_firewall_zone.zones[each.key].id
    matching_target = "ANY"
  }
}

# DNS: every zone -> infra Pi-hole on 53 (tcp+udp). Scope to the infra zone
# for now; tighten to the Pi-hole's 10.0.20.x IP once it is assigned.
resource "unifi_firewall_policy" "dns_to_pihole" {
  for_each = local.zone_networks
  name     = "DNS: ${each.key} -> infra pihole 53"
  action   = "ALLOW"
  protocol = "tcp_udp"

  source = {
    zone_id         = unifi_firewall_zone.zones[each.key].id
    matching_target = "ANY"
  }
  destination = {
    zone_id            = unifi_firewall_zone.zones["infra"].id
    matching_target    = "ANY"
    port               = "53"
    port_matching_type = "SPECIFIC"
  }
}

# Monitoring scrapes: monitoring -> each scrape target on 9100 (node-exporter)
# and 12345 (alloy). Excludes house (no node-exporter); monitoring itself is
# covered by the intra-zone ALLOW.
resource "unifi_firewall_policy" "monitoring_scrapes" {
  for_each = local.scrape_targets
  name     = "Scrape: monitoring -> ${each.key} 9100,12345"
  action   = "ALLOW"
  protocol = "tcp"

  source = {
    zone_id         = unifi_firewall_zone.zones["monitoring"].id
    matching_target = "ANY"
  }
  destination = {
    zone_id            = unifi_firewall_zone.zones[each.key].id
    matching_target    = "ANY"
    port               = "9100,12345"
    port_matching_type = "SPECIFIC"
  }
}

# SSH: workstations (dagon) -> every zone on 22.
resource "unifi_firewall_policy" "workstations_ssh" {
  for_each = local.zone_networks
  name     = "SSH: workstations -> ${each.key} 22"
  action   = "ALLOW"
  protocol = "tcp"

  source = {
    zone_id         = unifi_firewall_zone.zones["workstations"].id
    matching_target = "ANY"
  }
  destination = {
    zone_id            = unifi_firewall_zone.zones[each.key].id
    matching_target    = "ANY"
    port               = "22"
    port_matching_type = "SPECIFIC"
  }
}

# Proxmox API: workstations -> mgmt on 8006 (22 is covered by workstations_ssh).
resource "unifi_firewall_policy" "workstations_proxmox" {
  name     = "Proxmox: workstations -> mgmt 8006"
  action   = "ALLOW"
  protocol = "tcp"

  source = {
    zone_id         = unifi_firewall_zone.zones["workstations"].id
    matching_target = "ANY"
  }
  destination = {
    zone_id            = unifi_firewall_zone.zones["mgmt"].id
    matching_target    = "ANY"
    port               = "8006"
    port_matching_type = "SPECIFIC"
  }
}

# SMB: workstations -> storage (NAS) on 445 (22 is covered by workstations_ssh).
# Scope to the storage zone for now; tighten to the NAS 10.0.90.x IP after.
resource "unifi_firewall_policy" "workstations_nas_smb" {
  name     = "SMB: workstations -> storage NAS 445"
  action   = "ALLOW"
  protocol = "tcp"

  source = {
    zone_id         = unifi_firewall_zone.zones["workstations"].id
    matching_target = "ANY"
  }
  destination = {
    zone_id            = unifi_firewall_zone.zones["storage"].id
    matching_target    = "ANY"
    port               = "445"
    port_matching_type = "SPECIFIC"
  }
}