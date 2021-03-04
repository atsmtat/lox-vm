#[test]
fn test_number() {
    let mut out = Vec::new();
    lox_vm::interpret("print 3.14;", &mut out);
    let string_output = String::from_utf8(out).unwrap();
    assert_eq!("3.14", string_output.trim());
}

#[test]
fn test_precedence() {
    let mut out = Vec::new();
    let source = r#"
print 3 * 4 + 4 * 2; // 20
print 4 > -2 and -4 >= 2; // false
print 3 > 2 == -1 < -3 * -2; // true
print (-1 + 2) * 3 - -4; // 7
"#;

    lox_vm::interpret(source, &mut out);
    let string_output = String::from_utf8(out).unwrap();
    assert_eq!(
        r#"20
false
true
7"#,
        string_output.trim()
    );
}

#[test]
fn test_associative() {
    let mut out = Vec::new();
    let source = r#"
print 3 - 4 + 4 - 2; // (((3 - 4) + 4) - 2)
var a;
var b;
a = b = 42; // a = (b = c)
print a;
print b;
"#;

    lox_vm::interpret(source, &mut out);
    let string_output = String::from_utf8(out).unwrap();
    assert_eq!(
        r#"1
42
42"#,
        string_output.trim()
    );
}
