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

variable "ballooning" {
  # Not a bool with a default: the bpg provider has no `ballooning` attribute,
  # it exposes Proxmox's balloon as `memory.floating` (MB, 0 = off, and 0 is
  # the provider's own default).  null therefore means "do not touch what the
  # provider already does", so no existing module call changes behaviour; false
  # writes `floating = 0` explicitly, and true sets `floating = dedicated`,
  # which is upstream's documented way to turn ballooning ON.
  type        = bool
  default     = null
  description = "Proxmox memory ballooning (qm --balloon). null (default) leaves it to the provider, false means floating=0 i.e. no balloon device, true means floating=dedicated. Pass false for VMs with passed-through devices, and for any VM whose guest RAM is a cache the host must not take back — strata's model page cache on comfyui, for example: a balloon inflation there takes back exactly the memory the --mmap-experts expert tiers are using and puts the engine back to reading experts from NVMe."
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

variable "image_file" {
  type        = string
  default     = null
  description = "Path (local file or URL) to the per-VM QCOW2 that CI pulled from MinIO. When set, it is uploaded to the node's `local:import` datastore over the Proxmox HTTP API (proxmox_virtual_environment_file, content_type=import — API token is sufficient, no SSH), and the VM's boot disk is imported from it. Proxmox never fetches from MinIO itself (its download-url HEAD probe is rejected on presigned GET URLs). import_from is create-only."
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

variable "host_pci" {
  type = list(object({
    device = string
    id     = string
    pcie   = optional(bool, false)
    rombar = optional(bool, true)
    xvga   = optional(bool, false)
  }))
  default     = []
  description = "Host PCI devices to passthrough (GPUs etc). device is the Proxmox name 'hostpciX' (X 0-3); id is the PCI address (e.g. 01:00). Empty by default."
}
