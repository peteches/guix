module "prometheus" {
  source    = "./modules/proxmox-vm"
  vmid      = 100
  name      = "prometheus"
  node_name = "proxmox1"
  image_file = lookup(var.vm_image_files, "prometheus", null)

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
  node_name = "proxmox1"

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


module "loki" {
  source    = "./modules/proxmox-vm"
  vmid      = 103
  name      = "loki"
  node_name = "proxmox1"

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
  node_name = "proxmox1"

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
  node_name = "proxmox1"

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


module "prowlarr" {
  source    = "./modules/proxmox-vm"
  vmid      = 107
  name      = "prowlarr"
  node_name = "proxmox1"

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
  node_name = "proxmox1"

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
  node_name = "proxmox1"

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
  node_name = "proxmox1"

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
  node_name = "proxmox1"

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

}

module "critical-grind-outline" {
  source    = "./modules/proxmox-vm"
  vmid      = 116
  name      = "critical-grind-outline"
  node_name = "proxmox1"

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

}

module "plane" {
  source    = "./modules/proxmox-vm"
  vmid      = 117
  name      = "plane"
  node_name = "proxmox1"

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = true
  started       = true
  memory        = 8192

  cpu = {
    cores = 4
    # plane-live's sharp/libvips prebuilt binary requires x86-64-v2
    # (SSE4.2, POPCNT); qemu64 doesn't offer it and the container
    # crash-loops with "Unsupported CPU: Prebuilt binaries for Linux x64
    # require v2 microarchitecture".
    type = "x86-64-v2-AES"
  }

  efi_disk = {
    datastore_id      = "local-lvm"
    type              = "4m"
    pre_enrolled_keys = false
  }

  disk = {
    datastore_id = "local-lvm"
    interface    = "virtio0"
    size         = 50
  }

  network_device = {
    bridge = "vmbr0"
    model  = "virtio"
  }

  agent = {
    enabled = true
  }

  image_file = lookup(var.vm_image_files, "plane", null)

}

module "claude-workstation" {
  source    = "./modules/proxmox-vm"
  vmid      = 118
  name      = "claude-workstation"
  node_name = "proxmox3"

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = true
  started       = true
  memory        = 8192

  cpu = {
    cores = 4
    # claude-code ships a Node SEA whose bundled V8 assumes a modern CPU
    # baseline; the default "qemu64" model exposes no AVX/SSE4/AES/RDRAND and
    # the binary busy-spins forever at startup. "host" passes the node CPU's
    # full feature set through to the guest. Requires a VM power cycle to apply.
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
    size         = 100
  }

  network_device = {
    bridge = "vmbr0"
    model  = "virtio"
  }

  agent = {
    enabled = true
  }

  image_file = lookup(var.vm_image_files, "claude-workstation", null)

}

module "agents" {
  source    = "./modules/proxmox-vm"
  vmid      = 121
  name      = "agents"
  node_name = "proxmox3"

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = true
  started       = true
  memory        = 4096

  cpu = {
    cores = 4
    # sillytavern/rules-lawyer's Node native deps (sharp et al) require
    # x86-64-v2 (SSE4.2, POPCNT); qemu64 doesn't offer it.
    type = "x86-64-v2-AES"
  }

  efi_disk = {
    datastore_id      = "local-lvm"
    type              = "4m"
    pre_enrolled_keys = false
  }

  disk = {
    datastore_id = "local-lvm"
    interface    = "virtio0"
    # Must be >= the 25G qcow2 the image build pipeline produces, plus
    # headroom for the media the harnesses generate locally.
    size         = 75
  }

  network_device = {
    bridge = "vmbr0"
    model  = "virtio"
  }

  agent = {
    enabled = true
  }

  image_file = lookup(var.vm_image_files, "agents", null)

}

// ── Out-of-band VMs adopted into Terraform (2026-10-27) ─────────────────────
// These VMs predate the Terraform fleet and were created manually on proxmox3.
// They are imported (terraform import module.<name>.vm proxmox3/<vmid>) so that
// their node placement is pinned like the rest of the fleet and drift is
// visible in plans. The disk block is ignored post-create by the module, so the
// real (manually grown) disk sizes cause no churn.

module "concourse-db" {
  source    = "./modules/proxmox-vm"
  vmid      = 111
  name      = "concourse-db"
  node_name = "proxmox3"

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = true
  started       = true

  cpu    = { cores = 2, type = "kvm64" }
  memory = 2048

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

  agent = { enabled = true }
}

module "concourse-web01" {
  source    = "./modules/proxmox-vm"
  vmid      = 112
  name      = "concourse-web01"
  node_name = "proxmox3"

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = true
  started       = true

  cpu    = { cores = 2, type = "kvm64" }
  memory = 2048

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

  agent = { enabled = true }
}

module "concourse-worker01" {
  source    = "./modules/proxmox-vm"
  vmid      = 113
  name      = "concourse-worker01"
  node_name = "proxmox3"

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = true
  started       = true

  cpu    = { cores = 2, type = "host" }
  memory = 8096

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

  agent = { enabled = true }
}

module "guix-build" {
  source    = "./modules/proxmox-vm"
  vmid      = 119
  name      = "guix-build"
  node_name = "proxmox3"

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = true
  started       = true

  cpu    = { cores = 8, type = "qemu64" }
  memory = 12288

  efi_disk = {
    datastore_id      = "local-lvm"
    type              = "4m"
    pre_enrolled_keys = false
  }

  disk = {
    datastore_id = "local-lvm"
    interface    = "virtio0"
    size         = 75
  }

  network_device = {
    bridge = "vmbr0"
    model  = "virtio"
  }

  agent = { enabled = true }
}

module "comfyui" {
  source    = "./modules/proxmox-vm"
  vmid      = 120
  name      = "comfyui"
  node_name = "proxmox3"

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = true
  started       = true

  cpu    = { cores = 8, type = "host" }
  memory = 49152

  # GPU passthrough on proxmox3: the RTX 4090 (01:00) was already passed
  # through before the legacy import; the Intel Arc B70 (09:00) is the new
  # second GPU. Both are q35/pcie; rombar exposes the firmware ROM.
  host_pci = [
    { device = "hostpci0", id = "01:00", pcie = true, rombar = true },
    { device = "hostpci1", id = "09:00", pcie = true, rombar = true },
  ]

  efi_disk = {
    datastore_id      = "local-lvm"
    type              = "4m"
    pre_enrolled_keys = false
  }

  disk = {
    datastore_id = "local-lvm"
    interface    = "virtio0"
    size         = 150
  }

  network_device = {
    bridge = "vmbr0"
    model  = "virtio"
  }

  agent = { enabled = true }
}

# --- Legacy proxmox1 VMs (imported 2026-10-27; not previously in state) ---
# Configs match the live VMs (fetched via the Proxmox API); the disk block is
# ignored by the module lifecycle, so only node pinning and host placement are
# enforced. No cpu type was set on these VMs, so the effective type is the
# Proxmox default kvm64.

module "pihole" {
  source    = "./modules/proxmox-vm"
  vmid      = 102
  name      = "pihole"
  node_name = "proxmox1"

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = true
  started       = true

  cpu    = { cores = 2, type = "kvm64" }
  memory = 1024

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

  agent = { enabled = true }
}

module "caddy" {
  source    = "./modules/proxmox-vm"
  vmid      = 106
  name      = "caddy"
  node_name = "proxmox1"

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = true
  started       = true

  cpu    = { cores = 2, type = "kvm64" }
  memory = 1024

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

  agent = { enabled = true }
}

module "vault" {
  source    = "./modules/proxmox-vm"
  vmid      = 114
  name      = "vault"
  node_name = "proxmox1"

  bios          = "ovmf"
  machine       = "q35"
  scsi_hardware = "virtio-scsi-pci"
  on_boot       = true
  started       = true

  cpu    = { cores = 2, type = "kvm64" }
  memory = 2048

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

  agent = { enabled = true }
}
