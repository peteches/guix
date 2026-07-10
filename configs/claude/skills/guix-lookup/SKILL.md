# guix-lookup

Search for Guix system services, home services, or packages by name, then optionally locate and read their source `.scm` file.

## Usage

Invoke as `/guix-lookup <term>` — optionally specifying the type:
- `/guix-lookup openvpn` — searches all three categories
- `/guix-lookup --system openvpn` — system services only
- `/guix-lookup --home openssh` — home services only
- `/guix-lookup --package wireguard` — packages only

## Steps

### 1. Search system services

```bash
guix system search <term>
```

Filter with `recsel` to narrow results. For example, to show only the `location` field for entries whose name matches "client":

```bash
guix system search <term> | recsel -e 'name ~ "client"' -p name,location
```

### 2. Search home services

```bash
guix home search <term>
```

Same `recsel` filtering applies:

```bash
guix home search <term> | recsel -e 'name ~ "<filter>"' -p name,location
```

### 3. Search packages

```bash
guix package -s <term> | recsel -p name,location
```

To match an exact package name:

```bash
guix package -s <term> | recsel -e 'name = "<exact-name>"' -p name,location
```

### 4. Locate the source `.scm` file

The `location` field returns a relative path with a line number, e.g. `gnu/services/vpn.scm:540:2`. Strip the line numbers and scan `GUILE_LOAD_PATH` to find the actual file:

```bash
loc=$(guix system search <term> | recsel -e 'name = "<name>"' -p location \
      | awk -F': ' '{print $2}' | cut -d: -f1)

for i in ${GUILE_LOAD_PATH//:/ }; do
  ls "${i}/${loc}" 2>/dev/null && echo "Found: ${i}/${loc}"
done
```

Once found, read or open that file to inspect the service or package definition.

## recsel quick reference

| Goal | Flag |
|---|---|
| Print specific fields | `-p name,location,description` |
| Filter by substring match | `-e 'name ~ "pattern"'` |
| Filter by exact match | `-e 'name = "exact-name"'` |
| Combine filters | `-e 'name ~ "foo" AND relevance > 3'` |
