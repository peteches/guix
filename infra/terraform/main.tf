module "prometheus" {
  source    = "./modules/proxmox-vm"
  vmid      = 100
  name      = "prometheus"
  node_name = var.node_name

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = true
  started       = true
  memory        = 2048

  cpu = {
    cores = 1
    type  = "qemu64"
  }

  efi_disk = {
    datastore_id      = "local-lvm"
    type              = "4m"
    pre_enrolled_keys = false
  }

  disk = {
    datastore_id = "local-lvm"
    interface    = "virtio0"
    size         = 25
  }

  network_device = {
    bridge = "vmbr0"
    model  = "virtio"
  }

  agent = {
    enabled = true
  }
}

module "grafana" {
  source    = "./modules/proxmox-vm"
  vmid      = 101
  name      = "grafana"
  node_name = var.node_name

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = true
  started       = true
  memory        = 4096

  cpu = {
    cores = 2
    type  = "qemu64"
  }

  efi_disk = {
    datastore_id      = "local-lvm"
    type              = "4m"
    pre_enrolled_keys = false
  }

  disk = {
    datastore_id = "local-lvm"
    interface    = "virtio0"
    size         = 25
  }

  network_device = {
    bridge = "vmbr0"
    model  = "virtio"
  }

  agent = {
    enabled = true
  }
}

module "pihole" {
  source    = "./modules/proxmox-vm"
  vmid      = 102
  name      = "pihole"
  node_name = var.node_name

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = true
  started       = true
  memory        = 1024

  cpu = {
    cores = 2
    type  = "qemu64"
  }

  efi_disk = {
    datastore_id      = "local-lvm"
    type              = "4m"
    pre_enrolled_keys = false
  }

  disk = {
    datastore_id = "local-lvm"
    interface    = "virtio0"
    size         = 25
  }

  network_device = {
    bridge = "vmbr0"
    model  = "virtio"
  }

  agent = {
    enabled = true
  }
}

module "loki" {
  source    = "./modules/proxmox-vm"
  vmid      = 103
  name      = "loki"
  node_name = var.node_name

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = true
  started       = true
  memory        = 2048

  cpu = {
    cores = 1
    type  = "qemu64"
  }

  efi_disk = {
    datastore_id      = "local-lvm"
    type              = "4m"
    pre_enrolled_keys = false
  }

  disk = {
    datastore_id = "local-lvm"
    interface    = "virtio0"
    size         = 25
  }

  network_device = {
    bridge = "vmbr0"
    model  = "virtio"
  }

  agent = {
    enabled = true
  }
}

module "git" {
  source    = "./modules/proxmox-vm"
  vmid      = 104
  name      = "git"
  node_name = var.node_name

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = true
  started       = true
  memory        = 1024

  cpu = {
    cores = 1
    type  = "qemu64"
  }

  efi_disk = {
    datastore_id      = "local-lvm"
    type              = "4m"
    pre_enrolled_keys = false
  }

  disk = {
    datastore_id = "local-lvm"
    interface    = "virtio0"
    size         = 25
  }

  network_device = {
    bridge = "vmbr0"
    model  = "virtio"
  }

  agent = {
    enabled = true
  }
}

module "jellyfin" {
  source    = "./modules/proxmox-vm"
  vmid      = 105
  name      = "jellyfin"
  node_name = var.node_name

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = true
  started       = true
  memory        = 8192

  cpu = {
    cores = 4
    type  = "qemu64"
  }

  efi_disk = {
    datastore_id      = "local-lvm"
    type              = "4m"
    pre_enrolled_keys = false
  }

  disk = {
    datastore_id = "local-lvm"
    interface    = "virtio0"
    size         = 25
  }

  network_device = {
    bridge = "vmbr0"
    model  = "virtio"
  }

  agent = {
    enabled = true
  }
}

module "caddy" {
  source    = "./modules/proxmox-vm"
  vmid      = 106
  name      = "caddy"
  node_name = var.node_name

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = true
  started       = true
  memory        = 1024

  cpu = {
    cores = 2
    type  = "qemu64"
  }

  efi_disk = {
    datastore_id      = "local-lvm"
    type              = "4m"
    pre_enrolled_keys = false
  }

  disk = {
    datastore_id = "local-lvm"
    interface    = "virtio0"
    size         = 25
  }

  network_device = {
    bridge = "vmbr0"
    model  = "virtio"
  }

  agent = {
    enabled = true
  }
}

module "prowlarr" {
  source    = "./modules/proxmox-vm"
  vmid      = 107
  name      = "prowlarr"
  node_name = var.node_name

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = true
  started       = true
  memory        = 1024

  cpu = {
    cores = 2
    type  = "qemu64"
  }

  efi_disk = {
    datastore_id      = "local-lvm"
    type              = "4m"
    pre_enrolled_keys = false
  }

  disk = {
    datastore_id = "local-lvm"
    interface    = "virtio0"
    size         = 25
  }

  network_device = {
    bridge = "vmbr0"
    model  = "virtio"
  }

  agent = {
    enabled = true
  }
}

module "arr" {
  source    = "./modules/proxmox-vm"
  vmid      = 108
  name      = "arr"
  node_name = var.node_name

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = true
  started       = true
  memory        = 2048

  cpu = {
    cores = 2
    type  = "qemu64"
  }

  efi_disk = {
    datastore_id      = "local-lvm"
    type              = "4m"
    pre_enrolled_keys = false
  }

  disk = {
    datastore_id = "local-lvm"
    interface    = "virtio0"
    size         = 25
  }

  network_device = {
    bridge = "vmbr0"
    model  = "virtio"
  }

  agent = {
    enabled = true
  }
}

module "downloads" {
  source    = "./modules/proxmox-vm"
  vmid      = 109
  name      = "downloads"
  node_name = var.node_name

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = true
  started       = true
  memory        = 4096

  cpu = {
    cores = 4
    type  = "qemu64"
  }

  efi_disk = {
    datastore_id      = "local-lvm"
    type              = "4m"
    pre_enrolled_keys = false
  }

  disk = {
    datastore_id = "local-lvm"
    interface    = "virtio0"
    size         = 25
  }

  network_device = {
    bridge = "vmbr0"
    model  = "virtio"
  }

  agent = {
    enabled = true
  }
}

module "rustdesk" {
  source    = "./modules/proxmox-vm"
  vmid      = 110
  name      = "rustdesk"
  node_name = var.node_name

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = false
  started       = true
  memory        = 2048

  cpu = {
    cores = 2
    type  = "qemu64"
  }

  efi_disk = {
    datastore_id      = "local-lvm"
    type              = "4m"
    pre_enrolled_keys = false
  }

  disk = {
    datastore_id = "local-lvm"
    interface    = "virtio0"
    size         = 25
  }

  network_device = {
    bridge = "vmbr0"
    model  = "virtio"
  }

  agent = {
    enabled = true
  }
}

module "concourse-db" {
  source    = "./modules/proxmox-vm"
  vmid      = 111
  name      = "concourse-db"
  node_name = var.node_name

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = false
  started       = true
  memory        = 2048

  cpu = {
    cores = 2
    type  = "qemu64"
  }

  efi_disk = {
    datastore_id      = "local-lvm"
    type              = "4m"
    pre_enrolled_keys = false
  }

  disk = {
    datastore_id = "local-lvm"
    interface    = "virtio0"
    size         = 25
  }

  network_device = {
    bridge = "vmbr0"
    model  = "virtio"
  }

  agent = {
    enabled = true
  }
}

module "concourse-web01" {
  source    = "./modules/proxmox-vm"
  vmid      = 112
  name      = "concourse-web01"
  node_name = var.node_name

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = false
  started       = true
  memory        = 2048

  cpu = {
    cores = 2
    type  = "qemu64"
  }

  efi_disk = {
    datastore_id      = "local-lvm"
    type              = "4m"
    pre_enrolled_keys = false
  }

  disk = {
    datastore_id = "local-lvm"
    interface    = "virtio0"
    size         = 25
  }

  network_device = {
    bridge = "vmbr0"
    model  = "virtio"
  }

  agent = {
    enabled = true
  }
}

module "concourse-worker01" {
  source    = "./modules/proxmox-vm"
  vmid      = 113
  name      = "concourse-worker01"
  node_name = var.node_name

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = false
  started       = true
  memory        = 2048

  cpu = {
    cores = 2
    type  = "host"
  }

  efi_disk = {
    datastore_id      = "local-lvm"
    type              = "4m"
    pre_enrolled_keys = false
  }

  disk = {
    datastore_id = "local-lvm"
    interface    = "virtio0"
    size         = 25
  }

  network_device = {
    bridge = "vmbr0"
    model  = "virtio"
  }

  agent = {
    enabled = true
  }
}

module "vault" {
  source    = "./modules/proxmox-vm"
  vmid      = 114
  name      = "vault"
  node_name = var.node_name

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = false
  started       = true
  memory        = 2048

  cpu = {
    cores = 2
    type  = "qemu64"
  }

  efi_disk = {
    datastore_id      = "local-lvm"
    type              = "4m"
    pre_enrolled_keys = false
  }

  disk = {
    datastore_id = "local-lvm"
    interface    = "virtio0"
    size         = 25
  }

  network_device = {
    bridge = "vmbr0"
    model  = "virtio"
  }

  agent = {
    enabled = true
  }
}

module "critical-grind-campaign" {
  source    = "./modules/proxmox-vm"
  vmid      = 115
  name      = "critical-grind-campaign"
  node_name = var.node_name

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = true
  started       = true
  memory        = 2048

  cpu = {
    cores = 2
    type  = "qemu64"
  }

  efi_disk = {
    datastore_id      = "local-lvm"
    type              = "4m"
    pre_enrolled_keys = false
  }

  disk = {
    datastore_id = "local-lvm"
    interface    = "virtio0"
    size         = 30
  }

  network_device = {
    bridge = "vmbr0"
    model  = "virtio"
  }

  agent = {
    enabled = true
  }

  cloud_init = {
    datastore_id = "local-lvm"
    interface    = "ide0"

    ipv4_address = "192.168.51.202/23"
    ipv4_gateway = "192.168.50.1"

    ipv6_address = "2a10:d582:ef59::109/64"
    ipv6_gateway = "2a10:d582:ef59::1"

    dns_servers = [
      "192.168.51.189",
      "2a10:d582:ef59::101"
    ]
  }
}

module "critical-grind-outline" {
  source    = "./modules/proxmox-vm"
  vmid      = 116
  name      = "critical-grind-outline"
  node_name = var.node_name

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = true
  started       = true
  memory        = 2048

  cpu = {
    cores = 2
    type  = "qemu64"
  }

  efi_disk = {
    datastore_id      = "local-lvm"
    type              = "4m"
    pre_enrolled_keys = false
  }

  disk = {
    datastore_id = "local-lvm"
    interface    = "virtio0"
    size         = 30
  }

  network_device = {
    bridge = "vmbr0"
    model  = "virtio"
  }

  agent = {
    enabled = true
  }

  cloud_init = {
    datastore_id = "local-lvm"
    interface    = "ide0"

    ipv4_address = "192.168.51.203/23"
    ipv4_gateway = "192.168.50.1"

    ipv6_address = "2a10:d582:ef59::110/64"
    ipv6_gateway = "2a10:d582:ef59::1"

    dns_servers = [
      "192.168.51.189",
      "2a10:d582:ef59::101"
    ]
  }
}

