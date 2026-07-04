variable "vmid" {
  type        = number
  description = "Proxmox VM ID"
}

variable "name" {
  type        = string
  description = "VM name and hostname label"
}

variable "node_name" {
  type        = string
  description = "Proxmox cluster node name"
}

variable "template_vmid" {
  type        = number
  default     = 9000
  description = "VMID of the template to clone from when creating or recreating the VM"
}

variable "clone_full" {
  type        = bool
  default     = true
  description = "Whether to perform a full clone"
}

variable "clone_retries" {
  type        = number
  default     = 1
  description = "Clone retry count"
}

variable "bios" {
  type        = string
  description = "VM BIOS type, for example ovmf"
}

variable "machine" {
  type        = string
  description = "VM machine type, for example q35"
}

variable "scsi_hardware" {
  type        = string
  description = "SCSI controller type, for example virtio-scsi-pci"
}

variable "on_boot" {
  type        = bool
  description = "Whether the VM should start when the Proxmox node boots"
}

variable "started" {
  type        = bool
  description = "Whether Terraform should keep the VM running"
}

variable "cpu" {
  type = object({
    cores = number
    type  = string
  })

  description = "CPU configuration"
}

variable "memory" {
  type        = number
  description = "RAM in MB"
}

variable "efi_disk" {
  type = object({
    datastore_id      = string
    type              = string
    pre_enrolled_keys = bool
  })

  description = "EFI disk configuration"
}

variable "disk" {
  type = object({
    datastore_id = string
    interface    = string
    size         = number
  })

  description = "Primary VM disk configuration"
}

variable "network_device" {
  type = object({
    bridge = string
    model  = string
  })

  description = "Primary VM network device"
}

variable "agent" {
  type = object({
    enabled = bool
  })

  description = "QEMU guest agent configuration"
}

variable "image_url" {
  type        = string
  default     = null
  description = "HTTPS pre-signed URL of the per-VM QCOW2 image in MinIO. When set the VM is provisioned via disk import rather than template clone; the remote-exec provisioner is skipped because networking is baked into the Guix system image."
}

variable "cloud_init" {
  description = "Optional cloud-init network configuration for the VM."

  type = object({
    datastore_id = optional(string)
    interface    = optional(string)

    ipv4_address = optional(string)
    ipv4_gateway = optional(string)

    ipv6_address = optional(string)
    ipv6_gateway = optional(string)

    dns_servers = optional(list(string), [])
  })

  default = null

  validation {
    condition = var.cloud_init == null || try(
      var.cloud_init.ipv4_address != null || var.cloud_init.ipv6_address != null,
      false
    )
    error_message = "cloud_init must include ipv4_address, ipv6_address, or both."
  }
}
