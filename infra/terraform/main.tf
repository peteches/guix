module "prometheus" {
  source     = "./modules/proxmox-vm"
  vmid       = 100
  name       = "prometheus"
  node_name  = "proxmox1"
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
  started       = false
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
  started       = false
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
  memory        = 2048

  cpu = {
    # 2, not the 4 this file used to say: `qm config 118` has been running 2
    # for a while, and proxmox3 has only 32 physical cores with 36 vCPUs
    # allocated across its seven VMs, so the shrink was almost certainly made
    # by hand to leave CPU for comfyui's strata.  Recorded as it is live so
    # the next plan does not quietly raise it back — revisit it here if that
    # trade-off changes on purpose.
    cores = 2
    # claude-code ships a Node SEA whose bundled V8 assumes a modern CPU
    # baseline; the default "qemu64" model exposes no AVX/SSE4/AES/RDRAND and
    # the binary busy-spins forever at startup. "host" passes the node CPU's
    # full feature set through to the guest. Requires a VM power cycle to apply.
    type = "host"
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
    #
    # Recorded as it runs live (`qm config 121`), grown by hand on 2026-11-07:
    # the disk was 75G allocated but the guest's GPT partition and ext4 fs were
    # still the original 25G image, so the box hit 100% full.  See
    # proxmox-vms.org "resize the disk after import" for the recipe.
    # This value does NOT resize anything: ./modules/proxmox-vm ignores the
    # whole disk block post-create, so changing it here only documents live
    # state — the growth has to be done with `qm resize` + sfdisk + resize2fs.
    size = 150
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

  # Recorded as it runs live: `qm config 120` has 16 vCPUs while this file
  # still said 8, so applying the plan as-is would have halved strata's
  # threads in the very power cycle that raises the RAM — and this engine's
  # MoE prefill is plain SSE vector code (no AVX path), so its throughput
  # tracks the thread count.  Keep the live value; change CPU here on
  # purpose, not as plan drift.
  cpu    = { cores = 16, type = "host" }
  # 57344, not upstream's 65536: proxmox3 has 94 GiB and on 2026-11-03 had
  # 17 GiB free / 19 GiB available with comfyui already touching all 48 GiB it
  # was given.  A +16 GiB bump would put configured VM RAM (78.8 + 16 =
  # 94.8 GiB) past the physical host, so it would only work by leaning on
  # ZFS's 16 GiB ARC — the very thing that makes this VM's measured 2.1 GB/s
  # model reads possible — plus aggressive ballooning, and ballooning is the
  # one thing this VM must not have (see below).  64 GiB becomes the right
  # number once something else is trimmed: guix-build (VMID 119) is
  # configured 12288 and was touching 1.4 GiB.
  memory = 57344
  # Never inflate this VM's memory away: it passes through two GPUs, and PVE's
  # own guidance is balloon 0 with passed-through devices.  Here it is worse
  # than that — strata runs --mmap-experts, so its expert tiers live in the
  # guest's page cache and a balloon inflation takes back exactly the RAM that
  # keeps experts off the NVMe, putting strata back at the slow-prefill tier
  # the +8 GiB was bought to leave.  118 claude-workstation and 121 agents
  # already run balloon 0.  The provider has no `ballooning` attribute; this
  # variable maps to `memory.floating = 0` (no ballooning device) in
  # ./modules/proxmox-vm.  After applying, check `qm config 120` shows
  # `balloon: 0` — if it still has no `balloon:` line the provider did not
  # send the parameter, and it must be set once by hand:
  # `qm set 120 --balloon 0`.
  ballooning = false

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
