#[test]
fn test_capture_open() {
    let mut out = Vec::new();
    let source = r#"
fun outer() {
  var x = "outside";
  fun inner() {
    print x;
  }
  inner();
}
outer();
"#;
    lox_vm::interpret(source, &mut out);
    let string_output = String::from_utf8(out).unwrap();
    assert_eq!(r#""outside""#, string_output.trim());
}

#[test]
fn test_capture_close() {
    let mut out = Vec::new();
    let source = r#"
fun outer() {
  var x = "outside";
  fun inner() {
    print x;
  }
  return inner;
}

var inner = outer();
inner();
"#;
    lox_vm::interpret(source, &mut out);
    let string_output = String::from_utf8(out).unwrap();
    assert_eq!(r#""outside""#, string_output.trim());
}

#[test]
fn test_capture_update() {
    let mut out = Vec::new();
    let source = r#"
var globalSet;
var globalGet;

fun main() {
  var a = "initial";

  fun set() { a = "updated"; }
  fun get() { print a; }

  globalSet = set;
  globalGet = get;
}

main();
globalSet();
globalGet();
"#;
    lox_vm::interpret(source, &mut out);
    let string_output = String::from_utf8(out).unwrap();
    assert_eq!(r#""updated""#, string_output.trim());
}

#[test]
fn test_capture_nested() {
    let mut out = Vec::new();
    let source = r#"
fun outer() {
  var x = "value";
  fun middle() {
    fun inner() {
      print x;
    }

    print "create inner closure";
    return inner;
  }

  print "return from outer";
  return middle;
}

var mid = outer();
var in = mid();
in();
"#;
    lox_vm::interpret(source, &mut out);
    let string_output = String::from_utf8(out).unwrap();
    assert_eq!(
        r#""return from outer"
"create inner closure"
"value""#,
        string_output.trim()
    );
}
