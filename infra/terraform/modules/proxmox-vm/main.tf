resource "proxmox_virtual_environment_vm" "vm" {
  name      = var.name
  node_name = var.node_name
  vm_id     = var.vmid

  # Only used when creating a new VM — ignored on import of existing VMs.
  clone {
    vm_id = var.template_vmid
    full  = true
  }

  machine = "q35"
  bios    = "ovmf"
  on_boot = true
  started = true

  cpu {
    cores = var.cores
    type  = "x86-64-v2-AES"
  }

  memory {
    dedicated = var.memory
  }

  efi_disk {
    datastore_id      = var.storage
    type              = "4m"
    pre_enrolled_keys = false
  }

  disk {
    datastore_id = var.storage
    interface    = "virtio0"
    size         = var.disk_size
  }

  network_device {
    bridge = "vmbr0"
    model  = "virtio"
  }

  agent {
    enabled = true
  }
}
