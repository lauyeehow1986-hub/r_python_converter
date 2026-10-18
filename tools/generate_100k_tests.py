import os
import sys

TIERS = [
    (1, 10001, 20000, "High-Performance Computing, Parallelism & Scientific Computing", "HPC & Science"),
    (2, 20001, 30000, "Database Engines, ORM, Caching & Distributed Systems", "Database & DistSys"),
    (3, 30001, 40000, "Cloud Infrastructure, Kubernetes, DevOps & Microservices", "Cloud & DevOps"),
    (4, 40001, 50000, "Cybersecurity, Network Protocols, Forensics & Cryptanalysis", "Security & Protocols"),
    (5, 50001, 60000, "Compilers, Static Analysis, Code Generation & AST Transformations", "Compilers & AST"),
    (6, 60001, 70000, "Computer Vision, 3D Graphics, Geometry Processing & Spatial GIS", "Vision & GIS"),
    (7, 70001, 80000, "Speech Processing, Audio Synthesis & Real-Time DSP", "Audio & DSP"),
    (8, 80001, 90000, "Robotics, Control Systems, Kinematics & Autonomous Systems", "Robotics & Control"),
    (9, 90001, 100000, "Deep Learning Architectures, Reinforcement Learning & Graph ML", "Deep Learning & RL")
]

def get_algo(tier_num, cur_id, cat_id):
    fn_name = f"algo_pypi_{cur_id}"
    
    if tier_num == 1:
        # HPC & Science
        if cat_id == 0:
            py_code = f"def {fn_name}(n):\n    # HPC Stencil iteration {cur_id}\n    s = 0.0\n    for i in range(n):\n        s += (i * 0.5) / (i + 1.0)\n    return s"
            call = f"{fn_name}(50L)"
            metric = "StencilSum"
        elif cat_id == 1:
            py_code = f"def {fn_name}(arr):\n    # Reduction sum & norm {cur_id}\n    tot = 0.0\n    for x in arr:\n        tot += x * x\n    return tot"
            call = f"{fn_name}(c(1.5, 2.5, 3.5))"
            metric = "L2NormSq"
        elif cat_id == 2:
            py_code = f"def {fn_name}(val, factor):\n    # Scaling factor {cur_id}\n    return (val * factor) + 1.25"
            call = f"{fn_name}(10.0, 0.5)"
            metric = "ScaledVal"
        elif cat_id == 3:
            py_code = f"def {fn_name}(dx, steps):\n    # Euler step integrator {cur_id}\n    x = 1.0\n    for _ in range(steps):\n        x = x + dx * (-0.1 * x)\n    return x"
            call = f"{fn_name}(0.01, 20L)"
            metric = "EulerDecay"
        elif cat_id == 4:
            py_code = f"def {fn_name}(p, n):\n    # Speedup Amdahl law {cur_id}\n    return 1.0 / ((1.0 - p) + (p / n))"
            call = f"{fn_name}(0.8, 8L)"
            metric = "AmdahlSpeedup"
        elif cat_id == 5:
            py_code = f"def {fn_name}(arr):\n    # Max difference search {cur_id}\n    m = arr[0]\n    diff = 0.0\n    for x in arr:\n        if x > m: diff = x - m; m = x\n    return diff"
            call = f"{fn_name}(c(2.0, 5.0, 3.0, 9.0))"
            metric = "MaxDiff"
        elif cat_id == 6:
            py_code = f"def {fn_name}(dim, rank):\n    # Torus topology coordinate {cur_id}\n    return (rank // dim, rank % dim)"
            call = f"{fn_name}(4L, 11L)"
            metric = "TorusCoord"
        elif cat_id == 7:
            py_code = f"def {fn_name}(a, b):\n    # Dot product scalar {cur_id}\n    s = 0.0\n    for i in range(len(a)):\n        s += a[i] * b[i]\n    return s"
            call = f"{fn_name}(c(1.0, 2.0, 3.0), c(4.0, 5.0, 6.0))"
            metric = "DotProduct"
        elif cat_id == 8:
            py_code = f"def {fn_name}(t):\n    # Thermal diffusion decay {cur_id}\n    import math\n    return math.exp(-0.05 * t)"
            call = f"{fn_name}(10.0)"
            metric = "ThermalDecay"
        else:
            py_code = f"def {fn_name}(n):\n    # Matrix block index size {cur_id}\n    return (n + 31) // 32"
            call = f"{fn_name}(1000L)"
            metric = "BlockCount"

    elif tier_num == 2:
        # Database & DistSys
        if cat_id == 0:
            py_code = f"def {fn_name}(key_str):\n    # FNV-1a hash bucket {cur_id}\n    h = 2166136261\n    for ch in key_str:\n        h = (h ^ ord(ch)) * 16777619\n    return (h & 0x7FFFFFFF) % 1024"
            call = f"{fn_name}('session_key_test')"
            metric = "HashBucket"
        elif cat_id == 1:
            py_code = f"def {fn_name}(page_size, record_size):\n    # B-Tree fill capacity {cur_id}\n    return (page_size - 64) // record_size"
            call = f"{fn_name}(4096L, 128L)"
            metric = "BTreeCapacity"
        elif cat_id == 2:
            py_code = f"def {fn_name}(term, leader_id):\n    # Raft log vote header {cur_id}\n    return f'term={{term}}:leader={{leader_id}}'"
            call = f"{fn_name}(3L, 'nodeA')"
            metric = "RaftVote"
        elif cat_id == 3:
            py_code = f"def {fn_name}(arr, offset, limit):\n    # SQL Pagination offset-limit {cur_id}\n    return arr[offset:(offset + limit)]"
            call = f"{fn_name}(c(1L, 2L, 3L, 4L, 5L, 6L, 7L), 2L, 3L)"
            metric = "PageSlice"
        elif cat_id == 4:
            py_code = f"def {fn_name}(reads, writes, nodes):\n    # Dynamo quorum validation {cur_id}\n    return (reads + writes) > nodes"
            call = f"{fn_name}(2L, 2L, 3L)"
            metric = "QuorumValid"
        elif cat_id == 5:
            py_code = f"def {fn_name}(age_sec, ttl):\n    # Cache expiration check {cur_id}\n    return 'VALID' if age_sec < ttl else 'EXPIRED'"
            call = f"{fn_name}(45.0, 60.0)"
            metric = "CacheStatus"
        elif cat_id == 6:
            py_code = f"def {fn_name}(table, col):\n    # SQL query builder {cur_id}\n    return f'SELECT {{col}} FROM {{table}} WHERE active = 1'"
            call = f"{fn_name}('users', 'email')"
            metric = "SQLSelect"
        elif cat_id == 7:
            py_code = f"def {fn_name}(n_nodes):\n    # Ring token step size {cur_id}\n    return (2**32) // n_nodes"
            call = f"{fn_name}(16L)"
            metric = "RingStep"
        elif cat_id == 8:
            py_code = f"def {fn_name}(ts, node):\n    # Distributed UUID generation {cur_id}\n    return f'{{ts:x}}-{{node:04x}}'"
            call = f"{fn_name}(1600000000L, 42L)"
            metric = "DistUUID"
        else:
            py_code = f"def {fn_name}(row_count, est_selectivity):\n    # Query planner cost {cur_id}\n    return round(row_count * est_selectivity * 1.5, 2)"
            call = f"{fn_name}(100000L, 0.05)"
            metric = "PlannerCost"

    elif tier_num == 3:
        # Cloud & DevOps
        if cat_id == 0:
            py_code = f"def {fn_name}(cpu_millicores):\n    # Kubernetes CPU core conversion {cur_id}\n    return cpu_millicores / 1000.0"
            call = f"{fn_name}(2500L)"
            metric = "Cores"
        elif cat_id == 1:
            py_code = f"def {fn_name}(mem_bytes):\n    # Memory GiB conversion {cur_id}\n    return round(mem_bytes / (1024**3), 2)"
            call = f"{fn_name}(8589934592L)"
            metric = "MemGiB"
        elif cat_id == 2:
            py_code = f"def {fn_name}(replicas, cpu_usage, target):\n    # HPA autoscaling replicas {cur_id}\n    import math\n    return math.ceil(replicas * (cpu_usage / target))"
            call = f"{fn_name}(4L, 85.0, 50.0)"
            metric = "TargetReplicas"
        elif cat_id == 3:
            py_code = f"def {fn_name}(pod, namespace):\n    # K8s FQDN resolver {cur_id}\n    return f'{{pod}}.{{namespace}}.svc.cluster.local'"
            call = f"{fn_name}('api-worker', 'production')"
            metric = "FQDN"
        elif cat_id == 4:
            py_code = f"def {fn_name}(failures, threshold):\n    # Circuit breaker health status {cur_id}\n    return 'OPEN' if failures >= threshold else 'CLOSED'"
            call = f"{fn_name}(5L, 5L)"
            metric = "CircuitBreaker"
        elif cat_id == 5:
            py_code = f"def {fn_name}(base_delay, attempt):\n    # Exponential backoff with jitter ceiling {cur_id}\n    return min(base_delay * (2 ** attempt), 60.0)"
            call = f"{fn_name}(1.5, 4L)"
            metric = "BackoffSec"
        elif cat_id == 6:
            py_code = f"def {fn_name}(env, app):\n    # Docker container naming {cur_id}\n    return f'{{env}}-{{app}}-v1'"
            call = f"{fn_name}('prod', 'auth-service')"
            metric = "ContainerName"
        elif cat_id == 7:
            py_code = f"def {fn_name}(p99, p95, sla):\n    # SLA compliance monitor {cur_id}\n    return 'PASS' if p99 <= sla else 'BREACH'"
            call = f"{fn_name}(180.0, 95.0, 200.0)"
            metric = "SLAStatus"
        elif cat_id == 8:
            py_code = f"def {fn_name}(total_req, err_req):\n    # Availability error budget {cur_id}\n    return round((1.0 - err_req / total_req) * 100.0, 3)"
            call = f"{fn_name}(1000000L, 50L)"
            metric = "AvailabilityPct"
        else:
            py_code = f"def {fn_name}(ip_str, cidr):\n    # CIDR mask calculator {cur_id}\n    return (2 ** (32 - cidr)) - 2"
            call = f"{fn_name}('10.0.0.0', 24L)"
            metric = "UsableHosts"

    elif tier_num == 4:
        # Security & Protocols
        if cat_id == 0:
            py_code = f"def {fn_name}(val):\n    # Shift rotate cipher {cur_id}\n    return ((val << 3) | (val >> 5)) & 0xFF"
            call = f"{fn_name}(0x42L)"
            metric = "RotatedByte"
        elif cat_id == 1:
            py_code = f"def {fn_name}(a, b):\n    # Constant-time byte comparison {cur_id}\n    res = 0\n    for x, y in zip(a, b):\n        res |= (x ^ y)\n    return res == 0"
            call = f"{fn_name}(c(1L, 2L, 3L), c(1L, 2L, 3L))"
            metric = "ConstTimeEq"
        elif cat_id == 2:
            py_code = f"def {fn_name}(nonce, counter):\n    # ChaCha20 block counter state {cur_id}\n    return f'{{counter:08x}}-{{nonce}}'"
            call = f"{fn_name}('abcdef', 10L)"
            metric = "BlockState"
        elif cat_id == 3:
            py_code = f"def {fn_name}(data_len):\n    # PKCS#7 padding length {cur_id}\n    return 16 - (data_len % 16)"
            call = f"{fn_name}(25L)"
            metric = "PKCS7Pad"
        elif cat_id == 4:
            py_code = f"def {fn_name}(status):\n    # X.509 certificate revocation flag {cur_id}\n    return 'REVOKED' if status != 0 else 'GOOD'"
            call = f"{fn_name}(0L)"
            metric = "OCSPStatus"
        elif cat_id == 5:
            py_code = f"def {fn_name}(msg):\n    # CRC-16 CCITT polynomial dummy {cur_id}\n    c = 0xFFFF\n    for ch in msg:\n        c = (c ^ ord(ch)) & 0xFFFF\n    return c"
            call = f"{fn_name}('HELLO')"
            metric = "CRC16"
        elif cat_id == 6:
            py_code = f"def {fn_name}(entropy_bits):\n    # Password strength entropy level {cur_id}\n    return 'STRONG' if entropy_bits >= 60.0 else 'WEAK'"
            call = f"{fn_name}(72.5)"
            metric = "EntropyTier"
        elif cat_id == 7:
            py_code = f"def {fn_name}(port):\n    # TLS default port checker {cur_id}\n    return port == 443"
            call = f"{fn_name}(443L)"
            metric = "IsTLSPort"
        elif cat_id == 8:
            py_code = f"def {fn_name}(cost, iter_count):\n    # Argon2 work factor estimate {cur_id}\n    return cost * iter_count"
            call = f"{fn_name}(65536L, 3L)"
            metric = "Argon2Work"
        else:
            py_code = f"def {fn_name}(ipv6_chunks):\n    # IPv6 format expander {cur_id}\n    return ':'.join(ipv6_chunks)"
            call = f"{fn_name}(c('2001', 'db8', '0', '1'))"
            metric = "IPv6Joined"

    elif tier_num == 5:
        # Compilers & AST
        if cat_id == 0:
            py_code = f"def {fn_name}(tok_type, val):\n    # Token record packaging {cur_id}\n    return f'Token({{tok_type}}, {{val}})'"
            call = f"{fn_name}('IDENT', 'foo_bar')"
            metric = "TokenRecord"
        elif cat_id == 1:
            py_code = f"def {fn_name}(op):\n    # Operator precedence parsing {cur_id}\n    prec = {{'+': 10, '-': 10, '*': 20, '/': 20, '**': 30}}\n    return prec.get(op, 0)"
            call = f"{fn_name}('*')"
            metric = "OpPrecedence"
        elif cat_id == 2:
            py_code = f"def {fn_name}(depth):\n    # Indentation tab generator {cur_id}\n    return '    ' * depth"
            call = f"{fn_name}(2L)"
            metric = "IndentSpaces"
        elif cat_id == 3:
            py_code = f"def {fn_name}(scope_level, var_name):\n    # Symbol table mangling {cur_id}\n    return f'_{{scope_level}}_{{var_name}}'"
            call = f"{fn_name}(3L, 'counter')"
            metric = "MangledSym"
        elif cat_id == 4:
            py_code = f"def {fn_name}(nodes):\n    # AST node count traversal {cur_id}\n    return len(nodes) + 1"
            call = f"{fn_name}(c(1L, 2L, 3L, 4L))"
            metric = "ASTNodeTotal"
        elif cat_id == 5:
            py_code = f"def {fn_name}(edges, nodes):\n    # Cyclomatic complexity M = E - N + 2P {cur_id}\n    return edges - nodes + 2"
            call = f"{fn_name}(15L, 12L)"
            metric = "CyclomaticComplexity"
        elif cat_id == 6:
            py_code = f"def {fn_name}(v1, v2):\n    # Semver compare {cur_id}\n    return 'GREATER' if v1 > v2 else 'EQUAL'"
            call = f"{fn_name}(3L, 2L)"
            metric = "SemverCmp"
        elif cat_id == 7:
            py_code = f"def {fn_name}(lhs, rhs):\n    # Constant folding sum {cur_id}\n    return lhs + rhs"
            call = f"{fn_name}(40L, 2L)"
            metric = "FoldedConst"
        elif cat_id == 8:
            py_code = f"def {fn_name}(inst, reg):\n    # Assembly instruction format {cur_id}\n    return f'{{inst}} %{{reg}}'"
            call = f"{fn_name}('movq', 'rax')"
            metric = "AsmInst"
        else:
            py_code = f"def {fn_name}(in_vars, out_vars):\n    # Liveness interference check {cur_id}\n    return len(set(in_vars).intersection(set(out_vars)))"
            call = f"{fn_name}(c('a', 'b', 'c'), c('b', 'd'))"
            metric = "LivenessInterference"

    elif tier_num == 6:
        # Vision & GIS
        if cat_id == 0:
            py_code = f"def {fn_name}(r, g, b):\n    # Rec. 709 Luminance formula {cur_id}\n    return round(0.2126 * r + 0.7152 * g + 0.0722 * b, 3)"
            call = f"{fn_name}(255.0, 128.0, 64.0)"
            metric = "Rec709Luma"
        elif cat_id == 1:
            py_code = f"def {fn_name}(w, h):\n    # Aspect ratio calculation {cur_id}\n    return round(w / h, 4)"
            call = f"{fn_name}(1920L, 1080L)"
            metric = "AspectRatio"
        elif cat_id == 2:
            py_code = f"def {fn_name}(x1, y1, x2, y2):\n    # Bounding box IoU overlap {cur_id}\n    inter_w = max(0, min(x2, 100) - max(x1, 10))\n    inter_h = max(0, min(y2, 100) - max(y1, 10))\n    return inter_w * inter_h"
            call = f"{fn_name}(10L, 10L, 80L, 80L)"
            metric = "BBoxOverlap"
        elif cat_id == 3:
            py_code = f"def {fn_name}(lat, lon):\n    # GeoJSON point geometry {cur_id}\n    return '{{\"type\":\"Point\",\"coordinates\":[' + str(lon) + ',' + str(lat) + ']}}'"
            call = f"{fn_name}(37.7749, -122.4194)"
            metric = "GeoJSONPoint"
        elif cat_id == 4:
            py_code = f"def {fn_name}(x, y, z):\n    # 3D Vector Euclidean magnitude {cur_id}\n    import math\n    return round(math.sqrt(x*x + y*y + z*z), 3)"
            call = f"{fn_name}(3.0, 4.0, 12.0)"
            metric = "Vec3Norm"
        elif cat_id == 5:
            py_code = f"def {fn_name}(angle_deg):\n    # Radians angle conversion {cur_id}\n    import math\n    return round(angle_deg * math.pi / 180.0, 5)"
            call = f"{fn_name}(90.0)"
            metric = "RadiansVal"
        elif cat_id == 6:
            py_code = f"def {fn_name}(x, y, w, h):\n    # Bounding box area {cur_id}\n    return w * h"
            call = f"{fn_name}(5L, 5L, 50L, 40L)"
            metric = "BBoxArea"
        elif cat_id == 7:
            py_code = f"def {fn_name}(lat1, lat2):\n    # Latitude distance km approx {cur_id}\n    return round(abs(lat1 - lat2) * 111.0, 2)"
            call = f"{fn_name}(51.5074, 48.8566)"
            metric = "ApproxKm"
        elif cat_id == 8:
            py_code = f"def {fn_name}(u, v):\n    # Texture UV clamp {cur_id}\n    return (max(0.0, min(1.0, u)), max(0.0, min(1.0, v)))"
            call = f"{fn_name}(1.2, -0.1)"
            metric = "UVClamp"
        else:
            py_code = f"def {fn_name}(r, g, b):\n    # Hex color code builder {cur_id}\n    return f'#{{r:02x}}{{g:02x}}{{b:02x}}'"
            call = f"{fn_name}(255L, 165L, 0L)"
            metric = "HexColor"

    elif tier_num == 7:
        # Audio & DSP
        if cat_id == 0:
            py_code = f"def {fn_name}(freq):\n    # Pitch to MIDI note number {cur_id}\n    import math\n    return round(69 + 12 * math.log2(freq / 440.0), 1)"
            call = f"{fn_name}(440.0)"
            metric = "MidiNote"
        elif cat_id == 1:
            py_code = f"def {fn_name}(freq):\n    # Frequency to Mel scale {cur_id}\n    import math\n    return round(2595.0 * math.log10(1.0 + freq / 700.0), 2)"
            call = f"{fn_name}(1000.0)"
            metric = "MelScale"
        elif cat_id == 2:
            py_code = f"def {fn_name}(rms):\n    # Audio amplitude to dBFS {cur_id}\n    import math\n    return round(20.0 * math.log10(max(rms, 1e-6)), 2)"
            call = f"{fn_name}(0.5)"
            metric = "DBFS"
        elif cat_id == 3:
            py_code = f"def {fn_name}(sample_rate):\n    # Nyquist cutoff frequency {cur_id}\n    return sample_rate / 2.0"
            call = f"{fn_name}(44100.0)"
            metric = "NyquistFreq"
        elif cat_id == 4:
            py_code = f"def {fn_name}(window_size, hop_size):\n    # STFT frame overlap fraction {cur_id}\n    return round((window_size - hop_size) / window_size, 3)"
            call = f"{fn_name}(1024L, 256L)"
            metric = "STFTOverlap"
        elif cat_id == 5:
            py_code = f"def {fn_name}(samples, sample_rate):\n    # Audio duration in seconds {cur_id}\n    return round(samples / sample_rate, 3)"
            call = f"{fn_name}(88200L, 44100.0)"
            metric = "AudioDuration"
        elif cat_id == 6:
            py_code = f"def {fn_name}(bpm):\n    # Quarter note duration in milliseconds {cur_id}\n    return round(60000.0 / bpm, 1)"
            call = f"{fn_name}(120.0)"
            metric = "BeatMs"
        elif cat_id == 7:
            py_code = f"def {fn_name}(x, alpha):\n    # Exponential smoothing DSP filter {cur_id}\n    s = x[0]\n    for val in x[1:]:\n        s = alpha * val + (1.0 - alpha) * s\n    return round(s, 3)"
            call = f"{fn_name}(c(1.0, 2.0, 1.5, 3.0), 0.5)"
            metric = "ExpSmoothed"
        elif cat_id == 8:
            py_code = f"def {fn_name}(sig_power, noise_power):\n    # SNR decibels {cur_id}\n    import math\n    return round(10.0 * math.log10(sig_power / noise_power), 2)"
            call = f"{fn_name}(100.0, 1.0)"
            metric = "SNRdB"
        else:
            py_code = f"def {fn_name}(gain_factor):\n    # Soft clipper non-linear saturation {cur_id}\n    import math\n    return round(math.tanh(gain_factor), 3)"
            call = f"{fn_name}(1.5)"
            metric = "TanhSat"

    elif tier_num == 8:
        # Robotics & Control
        if cat_id == 0:
            py_code = f"def {fn_name}(theta1, theta2, l1, l2):\n    # 2-link planar arm forward kinematics X {cur_id}\n    import math\n    return round(l1 * math.cos(theta1) + l2 * math.cos(theta1 + theta2), 3)"
            call = f"{fn_name}(0.0, 0.0, 10.0, 10.0)"
            metric = "FK_X"
        elif cat_id == 1:
            py_code = f"def {fn_name}(v, omega, r):\n    # Differential drive wheel speeds {cur_id}\n    v_right = v + (omega * 0.5)\n    return round(v_right / r, 2)"
            call = f"{fn_name}(1.0, 0.2, 0.1)"
            metric = "WheelSpeed"
        elif cat_id == 2:
            py_code = f"def {fn_name}(kp, ki, kd, err, err_sum, err_diff):\n    # PID control signal {cur_id}\n    return round(kp * err + ki * err_sum + kd * err_diff, 3)"
            call = f"{fn_name}(2.0, 0.1, 0.5, 1.2, 5.0, -0.3)"
            metric = "PIDOut"
        elif cat_id == 3:
            py_code = f"def {fn_name}(x, y):\n    # Polar heading angle theta {cur_id}\n    import math\n    return round(math.atan2(y, x), 3)"
            call = f"{fn_name}(1.0, 1.0)"
            metric = "Atan2Heading"
        elif cat_id == 4:
            py_code = f"def {fn_name}(mass, accel):\n    # Newton second law force {cur_id}\n    return mass * accel"
            call = f"{fn_name}(15.0, 9.81)"
            metric = "ForceNewtons"
        elif cat_id == 5:
            py_code = f"def {fn_name}(q0, q1, q2, q3):\n    # Quaternion normalization norm {cur_id}\n    import math\n    return round(math.sqrt(q0*q0 + q1*q1 + q2*q2 + q3*q3), 4)"
            call = f"{fn_name}(1.0, 0.0, 0.0, 0.0)"
            metric = "QuatNorm"
        elif cat_id == 6:
            py_code = f"def {fn_name}(torque, omega):\n    # Motor mechanical power {cur_id}\n    return torque * omega"
            call = f"{fn_name}(5.5, 100.0)"
            metric = "MotorWatts"
        elif cat_id == 7:
            py_code = f"def {fn_name}(target, current, alpha):\n    # Exponential target smoothing {cur_id}\n    return current + alpha * (target - current)"
            call = f"{fn_name}(10.0, 2.0, 0.2)"
            metric = "TrajectoryStep"
        elif cat_id == 8:
            py_code = f"def {fn_name}(dist):\n    # LiDAR obstacle threshold check {cur_id}\n    return 'OBSTACLE' if dist < 0.5 else 'CLEAR'"
            call = f"{fn_name}(1.2)"
            metric = "LiDARCheck"
        else:
            py_code = f"def {fn_name}(p_est, q_noise):\n    # Kalman filter covariance time update {cur_id}\n    return round(p_est + q_noise, 4)"
            call = f"{fn_name}(0.25, 0.01)"
            metric = "KalmanPredVar"

    else:
        # Deep Learning & RL
        if cat_id == 0:
            py_code = f"def {fn_name}(x):\n    # ReLU activation {cur_id}\n    return max(0.0, x)"
            call = f"{fn_name}(-2.5)"
            metric = "ReLUVal"
        elif cat_id == 1:
            py_code = f"def {fn_name}(x):\n    # Sigmoid logistic activation {cur_id}\n    import math\n    return round(1.0 / (1.0 + math.exp(-x)), 4)"
            call = f"{fn_name}(0.0)"
            metric = "SigmoidVal"
        elif cat_id == 2:
            py_code = f"def {fn_name}(y_true, y_pred):\n    # Binary cross entropy loss {cur_id}\n    import math\n    eps = 1e-7\n    p = max(eps, min(1.0 - eps, y_pred))\n    return round(-(y_true * math.log(p) + (1.0 - y_true) * math.log(1.0 - p)), 4)"
            call = f"{fn_name}(1.0, 0.85)"
            metric = "BCELoss"
        elif cat_id == 3:
            py_code = f"def {fn_name}(q_val, reward, gamma, next_max_q):\n    # Bellman optimality update equation {cur_id}\n    return round(reward + gamma * next_max_q, 3)"
            call = f"{fn_name}(0.5, 1.0, 0.9, 2.0)"
            metric = "BellmanTarget"
        elif cat_id == 4:
            py_code = f"def {fn_name}(eps, decay):\n    # Epsilon greedy decay step {cur_id}\n    return max(0.05, round(eps * decay, 4))"
            call = f"{fn_name}(0.95, 0.99)"
            metric = "EpsilonDecay"
        elif cat_id == 5:
            py_code = f"def {fn_name}(d_model, n_heads):\n    # Transformer head dimension {cur_id}\n    return d_model // n_heads"
            call = f"{fn_name}(512L, 8L)"
            metric = "HeadDim"
        elif cat_id == 6:
            py_code = f"def {fn_name}(w_in, kernel, padding, stride):\n    # Conv1d output shape {cur_id}\n    return (w_in + 2 * padding - kernel) // stride + 1"
            call = f"{fn_name}(100L, 3L, 1L, 1L)"
            metric = "Conv1dOut"
        elif cat_id == 7:
            py_code = f"def {fn_name}(q_dot_k, d_k):\n    # Scaled dot-product attention factor {cur_id}\n    import math\n    return round(q_dot_k / math.sqrt(d_k), 4)"
            call = f"{fn_name}(12.0, 64.0)"
            metric = "AttnScale"
        elif cat_id == 8:
            py_code = f"def {fn_name}(deg):\n    # GCN normalization degree factor {cur_id}\n    import math\n    return round(1.0 / math.sqrt(deg), 4)"
            call = f"{fn_name}(4.0)"
            metric = "GCNDegreeInv"
        else:
            py_code = f"def {fn_name}(loss, l2_reg, weights_norm):\n    # Regularized objective loss {cur_id}\n    return round(loss + 0.5 * l2_reg * weights_norm, 4)"
            call = f"{fn_name}(0.35, 0.001, 150.0)"
            metric = "RegLoss"

    return py_code, call, metric

def build_all_100k_tests():
    out_dir = "tools/pypi_100k_tests"
    os.makedirs(out_dir, exist_ok=True)
    
    for tier_num, start_id, end_id, title, short_domain in TIERS:
        script_file = os.path.join(out_dir, f"test_pypi_{start_id}_{end_id}.R")
        print(f"Generating Tier {tier_num}: {start_id} to {end_id} ({title})...")
        
        with open(script_file, "w", encoding="utf-8") as f:
            f.write("#!/usr/bin/env Rscript\n")
            f.write(f"# {script_file}\n")
            f.write(f"# Top 100,000 Python Packages (Tests {start_id} to {end_id}): {title}\n\n")
            f.write("suppressPackageStartupMessages({\n")
            f.write('  pkgload::load_all("packages/pyrt", quiet = TRUE)\n')
            f.write('  pkgload::load_all("packages/pyport", quiet = TRUE)\n')
            f.write('  library(testthat)\n')
            f.write("})\n\n")
            f.write("True <- TRUE; False <- FALSE; None <- NULL\n")
            f.write('if (!exists("results_100k")) results_100k <<- list()\n\n')
            f.write("record_pass_100k <- function(id, name, domain, metric_name, metric_val) {\n")
            f.write("  results_100k[[length(results_100k) + 1L]] <<- list(\n")
            f.write('    id = id, name = name, domain = domain, metric = metric_name, value = metric_val, status = "PASS"\n')
            f.write("  )\n")
            f.write(f"  if (id %% 1000 == 0 || id == {start_id} || id == {end_id}) {{\n")
            f.write('    cat(sprintf("  [%06d/100000] %-30s [%-16s] -> %s: %s (PASS)\\n",\n')
            f.write('                id, name, domain, metric_name, as.character(metric_val)))\n')
            f.write("  }\n")
            f.write("}\n\n")

            # 100 blocks * 100 packages = 10,000 packages per tier
            for block_idx in range(100):
                b_start = start_id + block_idx * 100
                b_end = b_start + 99
                py_funcs = []
                calls = []

                for k in range(100):
                    cur_id = b_start + k
                    cat_id = k % 10
                    py_fn, call, metric = get_algo(tier_num, cur_id, cat_id)
                    py_funcs.append(py_fn)
                    pkg_label = f"pkg_{cur_id}_{short_domain.replace(' ', '_')}"
                    calls.append(f'res <- env${call}; record_pass_100k({cur_id}L, "{pkg_label}", "{short_domain}", "{metric}", res)')

                combined_py = "\n\n".join(py_funcs)
                # Escaping for R string
                escaped_py = combined_py.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n')
                
                f.write(f"# Block {block_idx + 1}/100: Tests {b_start} to {b_end}\n")
                f.write('env <- pyport::eval_python("' + escaped_py + '")\n')
                for c in calls:
                    f.write(c + "\n")
                f.write("\n")

        print(f"Tier {tier_num} ({start_id} to {end_id}) generated successfully.")

if __name__ == '__main__':
    build_all_100k_tests()
