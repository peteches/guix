variable "unifi_api_url" {
  type        = string
  description = "UCG API URL (no path — the provider auto-discovers paths), e.g. https://192.168.50.1"
  default     = "https://192.168.50.1"
}

# The UniFi API key is not a Terraform variable: the provider reads it from
# the UNIFI_API_KEY environment variable, which the Concourse task sets from
# the ((unify_api_key)) var. Keeping it out of variables.tf avoids a required
# var that would otherwise have to be passed on every plan/apply.