terraform {
  required_version = ">= 1.5"

  backend "s3" {
    bucket   = "terraform-state"
    key      = "unifi/terraform.tfstate"
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
    unifi = {
      source  = "ubiquiti-community/unifi"
      # Pinned to the stable 0.59.x line. 0.59.0 has every attribute this
      # config uses (subnet, dhcp_server.enabled, setting_preference,
      # purpose, enabled). The next registry release is 0.60.0-beta.0, which
      # we do not want driving a production gateway; bump this deliberately
      # once 0.60.0 is stable.
      version = "~> 0.59"
    }
  }
}

provider "unifi" {
  # api_key is read from the UNIFI_API_KEY environment variable (the provider's
  # documented env fallback); the Concourse task sets it via params. No
  # explicit api_key attribute here, so the env var is authoritative.
  api_url        = var.unifi_api_url
  allow_insecure = true
  site           = "default"
}
