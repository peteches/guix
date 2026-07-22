variable "proxmox_endpoint" {
  type        = string
  description = "Proxmox VE API endpoint, e.g. https://proxmox1.peteches.co.uk:8006/"
  default     = "https://proxmox1.ts.peteches.co.uk"
}

variable "proxmox_api_token" {
  type        = string
  sensitive   = true
  description = "Proxmox API token in 'user@pam!tokenid=secret' format. Set via TF_VAR_proxmox_api_token."
}

variable "node_name" {
  type        = string
  default     = "proxmox1"
  description = "Proxmox cluster node name"
}

variable "vm_image_files" {
  type        = map(string)
  default     = {}
  description = "Map of VM name to a local path (or URL) of its QCOW2, which CI pulled from MinIO. Passed via TF_VAR_vm_image_files='{\"plane\":\"/tmp/plane.qcow2\"}' during first provisioning only; the module uploads it to local:import over the Proxmox API and imports the boot disk from it. import_from is create-only, so once the VM exists this has no further effect."
}
