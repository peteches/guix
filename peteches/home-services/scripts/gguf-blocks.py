#!/usr/bin/env python3
import sys
try:
    import gguf  # ensure python-gguf is installed; otherwise exit(1)
except Exception:
    sys.exit(1)
path = sys.argv[1] if len(sys.argv) > 1 else ''
if not path:
    sys.exit(1)
try:
    r = gguf.GGUFReader(path)
    keys = [k for k in r.kv_data.keys() if k.endswith('block_count')]
    if not keys:
        sys.exit(1)
    print(int(r.kv_data[keys[0]]))
except Exception:
    sys.exit(1)