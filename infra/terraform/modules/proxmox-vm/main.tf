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

  dynamic "initialization" {
  for_each = var.cloud_init == null ? [] : [var.cloud_init]

  content {
    datastore_id = initialization.value.datastore_id
    interface    = initialization.value.interface

    dynamic "dns" {
      for_each = length(initialization.value.dns_servers) == 0 ? [] : [initialization.value.dns_servers]

      content {
        servers = dns.value
      }
    }

    ip_config {
      dynamic "ipv4" {
        for_each = initialization.value.ipv4_address == null ? [] : [initialization.value]

        content {
          address = ipv4.value.ipv4_address
          gateway = ipv4.value.ipv4_gateway
        }
      }

      dynamic "ipv6" {
        for_each = initialization.value.ipv6_address == null ? [] : [initialization.value]

        content {
          address = ipv6.value.ipv6_address
          gateway = ipv6.value.ipv6_gateway
        }
      }
    }
  }
}

  agent {
    enabled = var.agent.enabled
  }

  # Imported state contains an operating_system block, but no explicit type.
  # Keeping an empty block avoids Terraform trying to remove it or set type=l26.
  operating_system {}

}