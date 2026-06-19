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

variable "cores" {
  type        = number
  default     = 2
  description = "Number of vCPUs"
}

variable "memory" {
  type        = number
  default     = 2048
  description = "RAM in MB"
}

variable "disk_size" {
  type        = number
  default     = 25
  description = "virtio0 disk size in GiB"
}

variable "storage" {
  type        = string
  default     = "local-lvm"
  description = "Proxmox storage pool name"
}

variable "template_vmid" {
  type        = number
  default     = 9000
  description = "VMID of the template to clone from when creating a new VM"
}
