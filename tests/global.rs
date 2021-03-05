#[test]
fn test_def() {
    let mut out = Vec::new();
    let source = r#"
var boring_car;
var sports_car = "mcLaren F1";

print boring_car;
print sports_car;
"#;
    lox_vm::interpret(source, &mut out);
    let string_output = String::from_utf8(out).unwrap();
    assert_eq!(
        r#"nil
"mcLaren F1""#,
        string_output.trim()
    );
}

#[test]
fn test_get_set() {
    let mut out = Vec::new();
    let source = r#"
var my_car;
var sports_car = "mcLaren F1";
my_car = sports_car;

print my_car;
print sports_car;
"#;
    lox_vm::interpret(source, &mut out);
    let string_output = String::from_utf8(out).unwrap();
    assert_eq!(
        r#""mcLaren F1"
"mcLaren F1""#,
        string_output.trim()
    );
}

#[test]
fn test_late_bound() {
    let mut out = Vec::new();
    let source = r#"
fun my_car() {
   return sports_car;
}
var sports_car = "mcLaren F1";

print my_car();
"#;
    lox_vm::interpret(source, &mut out);
    let string_output = String::from_utf8(out).unwrap();
    assert_eq!(r#""mcLaren F1""#, string_output.trim());
}
