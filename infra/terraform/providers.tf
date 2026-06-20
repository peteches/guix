terraform {
  required_version = ">= 1.5"

  backend "s3" {
    bucket   = "terraform-state"
    key      = "proxmox-vms/terraform.tfstate"
    region   = "us-east-1"  # required by the S3 backend; ignored by MinIO

    endpoints = {
    	      s3 = "https://minio.ts.peteches.co.uk"
    }
    skip_requesting_account_id  = true 
    skip_credentials_validation = true
    skip_metadata_api_check     = true
    skip_region_validation      = true
    use_path_style              = true

    # Credentials are read from env vars at init time:
    #   export AWS_ACCESS_KEY_ID=<minio-access-key>
    #   export AWS_SECRET_ACCESS_KEY=<minio-secret-key>
  }

  required_providers {
    proxmox = {
      source  = "bpg/proxmox"
      version = "~> 0.73"
    }
    null = {
      source  = "hashicorp/null"
      version = "~> 3.0"
    }
  }
}

provider "proxmox" {
  endpoint  = var.proxmox_endpoint
  api_token = var.proxmox_api_token
  insecure  = false
}
