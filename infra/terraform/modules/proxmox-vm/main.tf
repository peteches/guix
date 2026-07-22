# The boot disk is imported from a qcow2 that CI (the terraform-apply task)
# pulled from MinIO and this resource uploads to the node's `local:import`
# datastore over the Proxmox HTTP API (content_type=import always uses the API,
# so the provider's API token is sufficient — no SSH, no root on the node).
# Proxmox never fetches from MinIO itself: proxmox_download_file makes Proxmox
# HEAD-probe the URL for metadata, and MinIO rejects a HEAD against a presigned
# GET URL (minio/minio#6146, "out of scope"), so that resource is unusable here.
# Requires the `import` content type enabled on the `local` datastore.
resource "proxmox_virtual_environment_file" "vm_image" {
  count        = var.image_file != null ? 1 : 0
  content_type = "import"
  datastore_id = "local"
  node_name    = var.node_name

  source_file {
    path      = var.image_file
    file_name = "${var.name}.qcow2"
  }

  lifecycle {
    # Upload once. import_from is create-only, so a later image build must not
    # churn this under an already-provisioned VM.
    ignore_changes = [source_file]
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
    import_from  = var.image_file != null ? proxmox_virtual_environment_file.vm_image[0].id : null
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
