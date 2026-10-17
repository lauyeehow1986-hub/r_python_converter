def parse_kv_lines(lines):
    out = {}
    for line in lines:
        s = line.strip()
        if len(s) > 0 and not s.startswith("#"):
            parts = s.split(":", 1)
            if len(parts) == 2:
                k = parts[0].strip()
                v = parts[1].strip()
                out[k] = v
    return out
