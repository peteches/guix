# Zone-based firewall: the nine migration zones and the /23 migration bridge.
#
# The nine unifi_firewall_zone resources move each VLAN network OUT of the
# system Internal zone into its own zone. After this apply the Internal zone
# holds only the /23 default network (192.168.50.0/23) -- the "migration
# bridge" endpoint. UniFi auto-creates a Block All catch-all for every
# custom-zone pair AND for intra-zone traffic, so inter-zone and intra-zone
# behaviour is default-deny unless explicitly ALLOWed. The explicit ALLOWs
# live in firewall-strict.tf and are applied together with this file.
#
# The /23 bridge below is a TEMPORARY permissive bidirectional rule between
# the Internal zone (/23) and every custom zone. It keeps /23<->zone traffic
# working while VMs are migrated one at a time and is removed once the /23 is
# empty. It is the migration safety net only -- do not widen it.

# The system Internal zone, which after the custom zones are created holds
# only the /23 default network. Looked up by name so the bridge can reference
# it without hardcoding the UUID.
data "unifi_firewall_zone" "internal" {
  name = "Internal"
}

# Zone name -> the unifi_network that lives in it.
locals {
  zone_networks = {
    mgmt           = unifi_network.mgmt
    infra          = unifi_network.infra
    monitoring     = unifi_network.monitoring
    media          = unifi_network.media
    critical-grind = unifi_network.critical_grind
    apps           = unifi_network.apps
    workstations   = unifi_network.workstations
    house          = unifi_network.house
    storage        = unifi_network.storage
  }
}

# The nine migration zones.
resource "unifi_firewall_zone" "zones" {
  for_each    = local.zone_networks
  name        = each.key
  network_ids = [each.value.id]
}

# /23 bridge: Internal -> each custom zone (any traffic). TEMPORARY.
resource "unifi_firewall_policy" "bridge_internal_to_zone" {
  for_each    = local.zone_networks
  name        = "MIGRATION /23 bridge: Internal -> ${each.key}"
  action      = "ALLOW"
  protocol    = "all"
  description = "Temporary: keep /23<->zone traffic working during migration. Remove once the /23 is empty."

  source = {
    zone_id         = data.unifi_firewall_zone.internal.id
    matching_target = "ANY"
  }
  destination = {
    zone_id         = unifi_firewall_zone.zones[each.key].id
    matching_target = "ANY"
  }
}

# /23 bridge: each custom zone -> Internal (any traffic). TEMPORARY.
resource "unifi_firewall_policy" "bridge_zone_to_internal" {
  for_each    = local.zone_networks
  name        = "MIGRATION /23 bridge: ${each.key} -> Internal"
  action      = "ALLOW"
  protocol    = "all"
  description = "Temporary: keep /23<->zone traffic working during migration. Remove once the /23 is empty."

  source = {
    zone_id         = unifi_firewall_zone.zones[each.key].id
    matching_target = "ANY"
  }
  destination = {
    zone_id         = data.unifi_firewall_zone.internal.id
    matching_target = "ANY"
  }
}
