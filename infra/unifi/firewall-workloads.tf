# Zone-based firewall: strict inter-VLAN ALLOWs -- part 2 (workloads, CI,
# reverse-proxy). See firewall-strict.tf for the baseline and the
# zone+port-scoping note that applies to every rule here.

# "workloads" = monitoring, media, critical-grind, apps (the locked ruleset's
# definition).
locals {
  workload_zones = {
    monitoring     = unifi_network.monitoring
    media          = unifi_network.media
    critical-grind = unifi_network.critical_grind
    apps           = unifi_network.apps
  }

  # Caddy reverse-proxy backend ports per destination zone.
  caddy_backend_ports = {
    media          = "8096,5055,9696,8989,7878,6789,9091"
    critical-grind = "8080,3000,80"
    apps           = "8188"
  }
}

# Source control: workloads -> infra git on 22. Scope to the infra zone for
# now; tighten to the git 10.0.20.x IP after.
resource "unifi_firewall_policy" "workloads_git" {
  for_each = local.workload_zones
  name     = "Git: ${each.key} -> infra 22"
  action   = "ALLOW"
  protocol = "tcp"

  source = {
    zone_id         = unifi_firewall_zone.zones[each.key].id
    matching_target = "ANY"
  }
  destination = {
    zone_id            = unifi_firewall_zone.zones["infra"].id
    matching_target    = "ANY"
    port               = "22"
    port_matching_type = "SPECIFIC"
  }
}

# Secrets: workloads -> infra vault on 8200. Scope to the infra zone for now;
# tighten to the vault 10.0.20.x IP after.
resource "unifi_firewall_policy" "workloads_vault" {
  for_each = local.workload_zones
  name     = "Vault: ${each.key} -> infra 8200"
  action   = "ALLOW"
  protocol = "tcp"

  source = {
    zone_id         = unifi_firewall_zone.zones[each.key].id
    matching_target = "ANY"
  }
  destination = {
    zone_id            = unifi_firewall_zone.zones["infra"].id
    matching_target    = "ANY"
    port               = "8200"
    port_matching_type = "SPECIFIC"
  }
}

# Guix substitutes + build offload: EVERY zone -> infra guix-build on 3000 and
# 22. Every system in peteches/systems/* is guix-based and builds against
# guix-build, so every zone needs this (confirmed by the user). Scope to the
# infra zone for now; tighten to the guix-build 10.0.20.x IP after.
resource "unifi_firewall_policy" "guix_build" {
  for_each = local.zone_networks
  name     = "Guix: ${each.key} -> infra 3000,22"
  action   = "ALLOW"
  protocol = "tcp"

  source = {
    zone_id         = unifi_firewall_zone.zones[each.key].id
    matching_target = "ANY"
  }
  destination = {
    zone_id            = unifi_firewall_zone.zones["infra"].id
    matching_target    = "ANY"
    port               = "3000,22"
    port_matching_type = "SPECIFIC"
  }
}

# Backups: workloads -> storage (NAS) on 22 for restic. Scope to the storage
# zone for now; tighten to the NAS 10.0.90.x IP after.
resource "unifi_firewall_policy" "workloads_nas_restic" {
  for_each = local.workload_zones
  name     = "Restic: ${each.key} -> storage 22"
  action   = "ALLOW"
  protocol = "tcp"

  source = {
    zone_id         = unifi_firewall_zone.zones[each.key].id
    matching_target = "ANY"
  }
  destination = {
    zone_id            = unifi_firewall_zone.zones["storage"].id
    matching_target    = "ANY"
    port               = "22"
    port_matching_type = "SPECIFIC"
  }
}

# CI deploys: infra concourse -> each workload on 22. Scope to the workload
# zone for now; tighten to the specific VM IPs after.
resource "unifi_firewall_policy" "concourse_deploys" {
  for_each = local.workload_zones
  name     = "Concourse: infra -> ${each.key} 22"
  action   = "ALLOW"
  protocol = "tcp"

  source = {
    zone_id         = unifi_firewall_zone.zones["infra"].id
    matching_target = "ANY"
  }
  destination = {
    zone_id            = unifi_firewall_zone.zones[each.key].id
    matching_target    = "ANY"
    port               = "22"
    port_matching_type = "SPECIFIC"
  }
}

# Reverse-proxy backends: infra caddy -> media/critical-grind/apps on the
# service ports. Scope to the destination zone for now; tighten to the
# specific backend VM IPs after.
resource "unifi_firewall_policy" "caddy_backends" {
  for_each = local.caddy_backend_ports
  name     = "Caddy: infra -> ${each.key} backends"
  action   = "ALLOW"
  protocol = "tcp"

  source = {
    zone_id         = unifi_firewall_zone.zones["infra"].id
    matching_target = "ANY"
  }
  destination = {
    zone_id            = unifi_firewall_zone.zones[each.key].id
    matching_target    = "ANY"
    port               = each.value
    port_matching_type = "SPECIFIC"
  }
}