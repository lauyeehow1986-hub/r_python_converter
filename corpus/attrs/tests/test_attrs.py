def test_record_evolve_and_asdict():
    r1 = Record({"x": 10, "y": 20})
    assert r1.get("x") == 10
    assert r1.get("z", 99) == 99
    r2 = r1.evolve({"y": 50, "z": 60})
    assert r1.get("y") == 20
    assert r2.get("y") == 50
    assert r2.get("z") == 60
    assert validate_range(15, 0, 20) == 15
