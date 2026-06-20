resource "proxmox_virtual_environment_vm" "vm" {
  name      = var.name
  node_name = var.node_name
  vm_id     = var.vmid

  clone {
    vm_id   = var.template_vmid
    full    = var.clone_full
    retries = var.clone_retries
  }

  bios          = var.bios
  machine       = var.machine
  scsi_hardware = var.scsi_hardware

  on_boot = var.on_boot
  started = var.started

  cpu {
    cores = var.cpu.cores
    type  = var.cpu.type
  }

  memory {
    dedicated = var.memory
  }

  efi_disk {
    datastore_id      = var.efi_disk.datastore_id
    type              = var.efi_disk.type
    pre_enrolled_keys = var.efi_disk.pre_enrolled_keys
  }

  disk {
    datastore_id = var.disk.datastore_id
    interface    = var.disk.interface
    size         = var.disk.size
  }

  network_device {
    bridge = var.network_device.bridge
    model  = var.network_device.model
  }

  agent {
    enabled = var.agent.enabled
  }

  # Imported state contains an operating_system block, but no explicit type.
  # Keeping an empty block avoids Terraform trying to remove it or set type=l26.
  operating_system {}

  lifecycle {
    prevent_destroy = true
  }
}