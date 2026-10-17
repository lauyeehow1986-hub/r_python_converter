class Record:
    def __init__(self, fields):
        self._fields = dict(fields)

    def get(self, key, default=None):
        if key in self._fields:
            return self._fields[key]
        return default

    def evolve(self, updates):
        merged = dict(self._fields)
        for k in updates:
            merged[k] = updates[k]
        return Record(merged)

    def asdict(self):
        return dict(self._fields)

    def keys(self):
        out = []
        for k in self._fields:
            out.append(k)
        return out


def validate_range(val, low, high):
    if val < low or val > high:
        raise ValueError("value out of range")
    return val
