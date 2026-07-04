resource "proxmox_download_file" "vm_image" {
  count        = var.image_url != null ? 1 : 0
  content_type = "import"
  datastore_id = "local"
  node_name    = var.node_name
  url          = var.image_url
  file_name    = "${var.name}.qcow2"
  overwrite    = false

  lifecycle {
    # After first download, ignore URL changes (presigned → stable URL, new builds).
    # overwrite=false ensures Proxmox never re-downloads when the file exists.
    ignore_changes = [url]
  }
}

resource "proxmox_virtual_environment_vm" "vm" {
  name      = var.name
  node_name = var.node_name
  vm_id     = var.vmid

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
    import_from  = var.image_url != null ? proxmox_download_file.vm_image[0].id : null
    interface    = var.disk.interface
    size         = var.disk.size
  }

  network_device {
    bridge = var.network_device.bridge
    model  = var.network_device.model
  }

  agent {
    enabled = true

    wait_for_ip {
      ipv4 = true
    }
  }

  # Keeping an empty block avoids Terraform trying to remove it or set type=l26.
  operating_system {}

  lifecycle {
    ignore_changes = [
      # import_from is create-only; never re-import disk from a new image build.
      disk[0].import_from,
      # clone was removed from config (deprecated path) but may still be in state
      # for existing VMs. Ignore it to avoid forced replacement during migration.
      clone,
    ]
  }
}
