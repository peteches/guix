output "vm_id" {
  value       = proxmox_virtual_environment_vm.vm.vm_id
  description = "Proxmox VM ID"
}

output "vm_name" {
  value       = proxmox_virtual_environment_vm.vm.name
  description = "VM name"
}
