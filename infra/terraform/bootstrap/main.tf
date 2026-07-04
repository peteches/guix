module "pihole" {
  source    = "../modules/proxmox-vm"
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

module "concourse-db" {
  source    = "../modules/proxmox-vm"
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
  source    = "../modules/proxmox-vm"
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
  source    = "../modules/proxmox-vm"
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

module "caddy" {
  source    = "../modules/proxmox-vm"
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

module "vault" {
  source    = "../modules/proxmox-vm"
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
