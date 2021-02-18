#[test]
fn test_number() {
    let mut out = Vec::new();
    lox_vm::interpret("print 3.14;", &mut out);
    let string_output = String::from_utf8(out).unwrap();
    assert_eq!("3.14", string_output.trim());
}
