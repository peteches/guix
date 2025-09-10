#!/usr/bin/env python3
import json
import shutil
import subprocess

def run(cmd):
    try:
        out = subprocess.check_output(cmd, stderr=subprocess.DEVNULL, text=True)
        return out.strip()
    except Exception:
        return ""

def parse_gpu_summary():
    """
    Returns:
      avg_util (int 0-100),
      used_mib (int),
      total_mib (int)
    Aggregates across multiple GPUs by averaging util and summing memory.
    """
    q = [
        "nvidia-smi",
        "--query-gpu=utilization.gpu,memory.used,memory.total",
        "--format=csv,noheader,nounits",
    ]
    out = run(q)
    if not out:
        return 0, 0, 0
    lines = [l.strip() for l in out.splitlines() if l.strip()]
    utils = []
    useds = []
    totals = []
    for line in lines:
        # Expected: "25, 1234, 24564"
        parts = [p.strip() for p in line.split(",")]
        if len(parts) != 3:
            continue
        try:
            util = int(parts[0])
            used = int(parts[1])   # MiB
            total = int(parts[2])  # MiB
            utils.append(util)
            useds.append(used)
            totals.append(total)
        except ValueError:
            continue
    if not totals:
        return 0, 0, 0
    avg_util = int(round(sum(utils) / max(1, len(utils))))
    used_mib = sum(useds)
    total_mib = sum(totals)
    return avg_util, used_mib, total_mib

def parse_top_processes(limit=5):
    """
    Tries compute-apps first (active contexts), then accounted-apps (recent usage).
    Returns list of (used_mib:int, pid:str, name:str) sorted by used_mib desc.
    """
    procs = []

    # Active compute contexts
    out = run([
        "nvidia-smi",
        "--query-compute-apps=pid,process_name,used_memory",
        "--format=csv,noheader,nounits",
    ])
    for line in out.splitlines():
        line = line.strip()
        if not line:
            continue
        parts = [p.strip() for p in line.split(",")]
        if len(parts) != 3:
            continue
        pid, name, used = parts
        try:
            used_mib = int(used)  # MiB already (nounits)
        except ValueError:
            continue
        procs.append((used_mib, pid, name))

    # If empty, fall back to accounted apps (avg usage over lifecycle)
    if not procs:
        out2 = run([
            "nvidia-smi",
            "--query-accounted-apps=pid,process_name,avg_memory_usage",
            "--format=csv,noheader,nounits",
        ])
        for line in out2.splitlines():
            line = line.strip()
            if not line:
                continue
            parts = [p.strip() for p in line.split(",")]
            if len(parts) != 3:
                continue
            pid, name, used = parts
            try:
                used_mib = int(used)
            except ValueError:
                continue
            procs.append((used_mib, pid, name))

    procs.sort(key=lambda x: x[0], reverse=True)
    return procs[:limit]

def mib_to_gib(mib):
    return mib / 1024.0

def main():
    # Check nvidia-smi availability
    if shutil.which("nvidia-smi") is None:
        print(json.dumps({
            "text": "GPU: n/a",
            "tooltip": "nvidia-smi not found in PATH",
            "class": "error"
        }))
        return

    util, used_mib, total_mib = parse_gpu_summary()
    if total_mib <= 0:
        print(json.dumps({
            "text": "GPU: n/a",
            "tooltip": "No NVIDIA GPU data available",
            "class": "error"
        }))
        return

    used_gib = mib_to_gib(used_mib)
    total_gib = mib_to_gib(total_mib)

    # Build tooltip with top processes
    top = parse_top_processes(limit=5)
    if top:
        lines = ["Top GPU memory users:"]
        for used, pid, name in top:
            lines.append(f"{used:>5} MiB  pid {pid}  {name}")
        tooltip = "\n".join(lines)
    else:
        tooltip = "No active GPU processes."

    # Severity classes for styling
    mem_pct = int(round((used_mib / total_mib) * 100))
    classes = []
    if util >= 90 or mem_pct >= 90:
        classes.append("critical")
    elif util >= 70 or mem_pct >= 70:
        classes.append("warning")
    else:
        classes.append("ok")

    text = f"{util}% {used_gib:.1f}/{total_gib:.0f} GiB"
    print(json.dumps({
        "text": text,
        "tooltip": tooltip,
        "class": " ".join(classes),
        # Optional: lets Waybar treat it like a percentage if you style it
        "percentage": util
    }))

if __name__ == "__main__":
    main()
