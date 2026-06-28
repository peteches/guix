locals {
  _pn_list = var.provisioned_network != null ? [var.provisioned_network] : []

  _ip_cmds = flatten([for pn in local._pn_list : compact(concat(
    [
      "sudo ip addr add ${pn.ipv4_address} dev ${pn.network_interface}",
      "sudo ip route replace default via ${pn.ipv4_gateway}",
    ],
    pn.ipv6_address != null ? [
      "sudo ip -6 addr add ${pn.ipv6_address} dev ${pn.network_interface}",
      "sudo ip -6 route replace default via ${pn.ipv6_gateway}",
    ] : []
  ))])
}

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
    enabled = true

    wait_for_ip {
      ipv4 = true
    }
  }

  connection {
    type        = "ssh"
    host        = flatten(self.ipv4_addresses)[0]
    user        = var.bootstrap_ssh_user
    private_key = file(var.bootstrap_ssh_private_key_path)
    timeout     = "10m"
  }

  # Keeping an empty block avoids Terraform trying to remove it or set type=l26.
  operating_system {}

  provisioner "remote-exec" {
    when = create

    connection {
      type = "ssh"
      host = try(
        self.ipv4_addresses[
          index(self.network_interface_names,
            coalesce(try(var.provisioned_network.network_interface, null), "eth0"))
        ][0],
        "0.0.0.0"
      )
      user        = "peteches"
      private_key = file(coalesce(try(var.provisioned_network.ssh_private_key_path, null), "/home/peteches/.ssh/id_ed25519"))
      timeout     = "3m"
    }

    inline = local._ip_cmds
  }
}
