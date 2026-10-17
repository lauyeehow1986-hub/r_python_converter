def test_parse_kv_lines():
    doc = ["# comment", "host: localhost", "port: 8080"]
    parsed = parse_kv_lines(doc)
    assert parsed["host"] == "localhost"
    assert parsed["port"] == "8080"
    assert yaml_indent_level(6, 2) == 3
