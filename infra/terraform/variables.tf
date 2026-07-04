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

variable "vm_image_urls" {
  type        = map(string)
  default     = {}
  description = "Map of VM name to pre-signed MinIO URL for its QCOW2 image. Passed via TF_VAR_vm_image_urls='{\"plane\":\"https://...\"}' during first provisioning only. After provisioning, commit a permanent image_url to main.tf for each VM."
}
