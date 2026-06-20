module "prometheus" {
  source    = "./modules/proxmox-vm"
  vmid      = 100
  name      = "prometheus"
  node_name = var.node_name
  cores     = 1
  memory    = 2048
  disk_size = 25
}

module "grafana" {
  source    = "./modules/proxmox-vm"
  vmid      = 101
  name      = "grafana"
  node_name = var.node_name
  cores     = 2
  memory    = 4096
  disk_size = 25
}

module "pihole" {
  source    = "./modules/proxmox-vm"
  vmid      = 102
  name      = "pihole"
  node_name = var.node_name
  cores     = 2
  memory    = 1024
  disk_size = 25
}

module "loki" {
  source    = "./modules/proxmox-vm"
  vmid      = 103
  name      = "loki"
  node_name = var.node_name
  cores     = 1
  memory    = 2048
  disk_size = 25
}

module "git" {
  source    = "./modules/proxmox-vm"
  vmid      = 104
  name      = "git"
  node_name = var.node_name
  cores     = 1
  memory    = 1024
  disk_size = 25
}

module "jellyfin" {
  source    = "./modules/proxmox-vm"
  vmid      = 105
  name      = "jellyfin"
  node_name = var.node_name
  cores     = 4
  memory    = 8192
  disk_size = 25
}

module "caddy" {
  source    = "./modules/proxmox-vm"
  vmid      = 106
  name      = "caddy"
  node_name = var.node_name
  cores     = 2
  memory    = 1024
  disk_size = 25
}

module "prowlarr" {
  source    = "./modules/proxmox-vm"
  vmid      = 107
  name      = "prowlarr"
  node_name = var.node_name
  cores     = 2
  memory    = 1024
  disk_size = 25
}

module "arr" {
  source    = "./modules/proxmox-vm"
  vmid      = 108
  name      = "arr"
  node_name = var.node_name
  cores     = 2
  memory    = 2048
  disk_size = 25
}

module "downloads" {
  source    = "./modules/proxmox-vm"
  vmid      = 109
  name      = "downloads"
  node_name = var.node_name
  cores     = 4
  memory    = 4096
  disk_size = 25
}

module "rustdesk" {
  source    = "./modules/proxmox-vm"
  vmid      = 110
  name      = "rustdesk"
  node_name = var.node_name
  cores     = 2
  memory    = 2048
  disk_size = 25
}

module "concourse-db" {
  source    = "./modules/proxmox-vm"
  vmid      = 111
  name      = "concourse-db"
  node_name = var.node_name
  cores     = 2
  memory    = 2048
  disk_size = 25
}

module "concourse-web01" {
  source    = "./modules/proxmox-vm"
  vmid      = 112
  name      = "concourse-web01"
  node_name = var.node_name
  cores     = 2
  memory    = 2048
  disk_size = 25
}

module "concourse-worker01" {
  source    = "./modules/proxmox-vm"
  vmid      = 113
  name      = "concourse-worker01"
  node_name = var.node_name
  cores     = 2
  memory    = 2048
  disk_size = 25
}

module "vault" {
  source    = "./modules/proxmox-vm"
  vmid      = 114
  name      = "vault"
  node_name = var.node_name
  cores     = 2
  memory    = 2048
  disk_size = 25
}
