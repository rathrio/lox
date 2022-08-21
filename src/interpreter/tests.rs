use crate::parser::Parser;
use std::io;

use super::*;

fn interpret_expr(input: &str) -> Result<Value> {
    let expr = Parser::parse_expr_str(input).expect("syntax error");
    let mut interpreter = Interpreter::new(io::stdout());
    let env = Rc::new(RefCell::new(Env::new(None)));
    interpreter.interpret_expr(&expr, env)
}

fn interpret(input: &str, out: &mut impl Write) -> Result<ControlFlow> {
    let program = Parser::parse_str(input).expect("syntax error");
    let mut interpreter = Interpreter::new(out);
    interpreter.interpret(&program)
}

fn assert_outputted(out: Vec<u8>, expected_output: String) {
    assert_eq!(
        String::from_utf8(out).unwrap(),
        format!("{}\n", expected_output)
    );
}

#[test]
fn test_unary() {
    assert_eq!(Value::Number(-42.0), interpret_expr("- 42").unwrap());
    assert_eq!(Value::Number(42.0), interpret_expr("--42").unwrap());

    assert_eq!(Value::Bool(false), interpret_expr("!true").unwrap());
    assert_eq!(Value::Bool(true), interpret_expr("!!true").unwrap());
    assert_eq!(Value::Bool(true), interpret_expr("!false").unwrap());
    assert_eq!(Value::Bool(true), interpret_expr("!nil").unwrap());
    assert_eq!(Value::Bool(false), interpret_expr("!42").unwrap());
    assert_eq!(
        Value::Bool(false),
        interpret_expr("!(\"hi\" + \"there\")").unwrap()
    );

    assert!(interpret_expr(r#"- false"#).is_err());
}

#[test]
fn test_add() {
    assert_eq!(Value::Number(3.0), interpret_expr("1 + 2").unwrap());
    assert_eq!(Value::Number(6.0), interpret_expr("1 + 2 + 3").unwrap());
    assert_eq!(
        Value::Str("Well, hello there friends!".into()),
        interpret_expr(r#""Well," + " " + "hello there friends" + "!""#).unwrap()
    );

    assert!(interpret_expr("true + 23").is_err());
}

#[test]
fn test_subtract() {
    assert_eq!(Value::Number(-1.0), interpret_expr("1 - 2").unwrap());
    assert!(interpret_expr("true - 23").is_err());
}

#[test]
fn test_mult() {
    assert_eq!(Value::Number(6.0), interpret_expr("2 * 3").unwrap());
    assert!(interpret_expr("\"chabis\" * 23").is_err());
}

#[test]
fn test_precedence() {
    assert_eq!(Value::Number(7.0), interpret_expr("1 + 2 * 3").unwrap());
    assert_eq!(Value::Number(9.0), interpret_expr("(1 + 2) * 3").unwrap());
}

#[test]
fn test_div() {
    assert_eq!(
        Value::Number(0.6666666666666666),
        interpret_expr("2 / 3").unwrap()
    );
    assert!(interpret_expr("\"chabis\" / 23").is_err());
}

#[test]
fn test_div_by_zero() {
    assert!(interpret_expr("42 / 0").is_err());
}

#[test]
fn test_equal() {
    assert_eq!(Value::Bool(true), interpret_expr("1 == 1").unwrap());
    assert_eq!(Value::Bool(false), interpret_expr("1 != 1").unwrap());

    assert_eq!(Value::Bool(false), interpret_expr("1 == 42").unwrap());
    assert_eq!(Value::Bool(true), interpret_expr("1 != 42").unwrap());

    assert_eq!(Value::Bool(false), interpret_expr("1 == \"1\"").unwrap());
    assert_eq!(Value::Bool(true), interpret_expr("1 != \"1\"").unwrap());

    assert_eq!(Value::Bool(false), interpret_expr("nil == false").unwrap());
    assert_eq!(Value::Bool(true), interpret_expr("nil != false").unwrap());

    assert_eq!(
        Value::Bool(true),
        interpret_expr("\"hi\" == \"hi\"").unwrap()
    );
    assert_eq!(
        Value::Bool(false),
        interpret_expr("\"hi\" != \"hi\"").unwrap()
    );

    assert_eq!(
        Value::Bool(false),
        interpret_expr("\"there\" == \"hi\"").unwrap()
    );
    assert_eq!(
        Value::Bool(true),
        interpret_expr("\"there\" != \"hi\"").unwrap()
    );

    assert_eq!(Value::Bool(true), interpret_expr("nil == nil").unwrap());
    assert_eq!(Value::Bool(false), interpret_expr("nil != nil").unwrap());
}

#[test]
fn test_comparisons() {
    assert_eq!(Value::Bool(true), interpret_expr("2 > 1").unwrap());
    assert_eq!(Value::Bool(true), interpret_expr("1 <= 1").unwrap());
    assert_eq!(Value::Bool(false), interpret_expr("1 < 1").unwrap());

    assert!(interpret_expr("1 > false").is_err());
}

#[test]
fn test_ternary() {
    assert_eq!(
        Value::Number(42.0),
        interpret_expr("4 > 3 ? 2 + 20 * 2 : nil").unwrap()
    );

    assert_eq!(
        Value::Str("alternate".into()),
        interpret_expr("nil ? 1 + 2 : \"alternate\"").unwrap()
    );
}

#[test]
fn test_comma() {
    assert_eq!(
        Value::Number(92.0),
        interpret_expr("1 == 1, 4 * 23").unwrap()
    );
}

#[test]
fn test_out() {
    let mut out = Vec::new();
    interpret("print \"Hello\" + \" World!\";", &mut out).unwrap();
    assert_outputted(out, "\"Hello World!\"".into());
}

#[test]
fn test_variable_decls() {
    let mut out = Vec::new();
    interpret("var a = 1; print a;", &mut out).unwrap();
    assert_outputted(out, "1".into());

    let mut out = Vec::new();
    interpret("var a = 1; var b = 2; print a + b;", &mut out).unwrap();
    assert_outputted(out, "3".into());
}

#[test]
fn test_variable_assign() {
    let mut out = Vec::new();
    interpret("var a = 1; a = 2 + a; print a;", &mut out).unwrap();
    assert_outputted(out, "3".into());
}

#[test]
fn test_block_scope() {
    let mut out = Vec::new();
    let script = r#"
        var a = "outer";
        {
            var a = "inner";
            print a;
        }
        print a;
        "#;
    interpret(script, &mut out).unwrap();
    assert_outputted(out, "\"inner\"\n\"outer\"".into());
}

#[test]
fn test_block_scope_2() {
    let mut out = Vec::new();
    let script = r#"
        var a = "outer";
        {
            {
                a = "inner";
            }
        }
        print a;
        "#;
    interpret(script, &mut out).unwrap();
    assert_outputted(out, "\"inner\"".into());
}

#[test]
fn test_block_scope_3() {
    let mut out = Vec::new();
    let script = r#"
        var a = "outer";
        {
            var b = "inner";
        }
        print b;
        "#;
    assert!(interpret(script, &mut out).is_err());
}

#[test]
fn test_block_scope_4() {
    let mut out = Vec::new();
    let script = r#"
        var a = 1;
        {
             var a = a + 2;
             print a;
        }
        "#;
    interpret(script, &mut out).unwrap();
    assert_outputted(out, "3".into());
}

#[test]
fn test_if_else() {
    let mut out = Vec::new();
    let script = r#"
        if (42)
            print "hey";
        else
            print "ho";
        "#;
    interpret(script, &mut out).unwrap();
    assert_outputted(out, "\"hey\"".into());

    let mut out = Vec::new();
    let script = r#"
        if (!42)
            print "hey";
        else {
            print "chabis";
        }
        "#;
    interpret(script, &mut out).unwrap();
    assert_outputted(out, "\"chabis\"".into());
}

#[test]
fn test_logical() {
    assert_eq!(Value::Number(42.0), interpret_expr("nil or 42").unwrap());
    assert_eq!(Value::Number(12.0), interpret_expr("true and 12").unwrap());
    assert_eq!(Value::Bool(false), interpret_expr("false and 12").unwrap());
}

#[test]
fn test_logical_2() {
    let mut out = Vec::new();
    let script = r#"
        var a = nil;
        var b = a or 42;
        print b;
        "#;
    interpret(script, &mut out).unwrap();
    assert_outputted(out, "42".into());
}

#[test]
fn test_while() {
    let mut out = Vec::new();
    let script = r#"
        var a = 3;
        while (a >= 0) {
            print a;
            a = a - 1;
        }
        "#;
    interpret(script, &mut out).unwrap();
    assert_outputted(out, "3\n2\n1\n0".into());
}

#[test]
fn test_break() {
    let mut out = Vec::new();
    let script = r#"
        var a = 3;
        while (true) {
            if (a < 0) break;
            print a;
            a = a - 1;
        }
        "#;
    interpret(script, &mut out).unwrap();
    assert_outputted(out, "3\n2\n1\n0".into());
}

#[test]
fn test_functions() {
    let mut out = Vec::new();
    let script = r#"
        fun count(n) {
            if (n > 1) count(n - 1);
            print n;
        }
        count(3);
        "#;
    interpret(script, &mut out).unwrap();
    assert_outputted(out, "1\n2\n3".into());
}

#[test]
fn test_return() {
    let mut out = Vec::new();
    let script = r#"
        fun add(a, b) {
            return a + b;
            // Unreachable
            return 42;
        }
        var c = add(3, 4);
        print c;
        "#;
    interpret(script, &mut out).unwrap();
    assert_outputted(out, "7".into());
}

#[test]
fn test_local_functions() {
    let mut out = Vec::new();
    let script = r#"
        fun makeCounter() {
            var i = 0;
            fun count() {
              i = i + 1;
              print i;
            }

            return count;
        }

        var counter = makeCounter();
        counter();
        counter();
        "#;
    interpret(script, &mut out).unwrap();
    assert_outputted(out, "1\n2".into());
}

#[test]
fn test_anon_functions() {
    let mut out = Vec::new();
    let script = r#"
        var fn = fun (a, b) { return a + b; };
        print fn(1, 2);
        "#;
    interpret(script, &mut out).unwrap();
    assert_outputted(out, "3".into());

    let mut out = Vec::new();
    let script = r#"
        fun thrice(fn) {
          for (var i = 1; i <= 3; i = i + 1) {
            fn(i);
          }
        }

        thrice(fun (a) {
          print a;
        });
        "#;
    interpret(script, &mut out).unwrap();
    assert_outputted(out, "1\n2\n3".into());
}

#[test]
fn test_closure_binding() {
    let mut out = Vec::new();
    let script = r#"
        var a = "global";
        {
            fun showA() {
                print a;
            }

            showA();
            var a = "block";
            showA();
        }
        "#;

    interpret(script, &mut out).unwrap();
    assert_outputted(out, "\"global\"\n\"global\"".into());
}

#[test]
fn test_global_bindings_can_by_resolved_without_old_ast() {
    let mut out = Vec::new();
    let mut interpreter = Interpreter::new(&mut out);
    interpreter
        .interpret(&Parser::parse_str("var a = 3.1415;").unwrap())
        .unwrap();

    interpreter
        .interpret(&Parser::parse_str("print a;").unwrap())
        .unwrap();

    assert_outputted(out, "3.1415".into());
}

#[test]
fn test_class() {
    let mut out = Vec::new();
    let script = r#"
        class DevonshireCream {
           serveOn() {
                return "Scones";
           }
        }
        print DevonshireCream;
        "#;

    interpret(script, &mut out).unwrap();
    assert_outputted(out, "DevonshireCream".into());
}

#[test]
fn test_constructors() {
    let mut out = Vec::new();
    let script = r#"
        class Bagel {}
        var bagel = Bagel();
        print bagel;
        "#;

    interpret(script, &mut out).unwrap();
    assert_outputted(out, "Bagel instance".into());
}

#[test]
fn test_properties() {
    let mut out = Vec::new();
    let script = r#"
        class Bagel {}
        var bagel = Bagel();
        bagel.cheddar = true;
        print bagel.cheddar;
        "#;

    interpret(script, &mut out).unwrap();
    assert_outputted(out, "true".into());
}

#[test]
fn test_fibonacci() {
    let mut out = Vec::new();
    let script = r#"
    var a = 0;
    var temp;

    for (var b = 1; a < 10; b = temp + b) {
        print a;
        temp = a;
        a = b;
    }
    "#;

    interpret(script, &mut out).unwrap();
    assert_outputted(out, "0\n1\n1\n2\n3\n5\n8".into());
}

#[test]
fn test_method() {
    let mut out = Vec::new();
    let script = r#"
    class Bacon {
        eat() {
          print "Crunch crunch crunch!";
        }
    }

    Bacon().eat();
    "#;

    interpret(script, &mut out).unwrap();
    assert_outputted(out, "\"Crunch crunch crunch!\"".into());
}

#[test]
fn test_this() {
    let mut out = Vec::new();
    let script = r#"
    class Cake {
        taste() {
          var adjective = "delicious";
          print "The " + this.flavor + " cake is " + adjective + "!";
        }
    }

    var cake = Cake();
    cake.flavor = "German chocolate";
    cake.taste();
    "#;

    interpret(script, &mut out).unwrap();
    assert_outputted(out, "\"The German chocolate cake is delicious!\"".into());
}

#[test]
fn test_this_in_callback() {
    let mut out = Vec::new();
    let script = r#"
    class Thing {
        getCallback() {
          fun localFunction() {
            print this;
          }

          return localFunction;
        }
    }

    var callback = Thing().getCallback();
    callback();
    "#;

    interpret(script, &mut out).unwrap();
    assert_outputted(out, "Thing instance".into());
}

#[test]
fn test_init() {
    let mut out = Vec::new();
    let script = r#"
    class Dog {
        init(name) {
            this.name = name;
        }
    }

    var dog = Dog("Fido");
    print dog.name;
    "#;

    interpret(script, &mut out).unwrap();
    assert_outputted(out, "\"Fido\"".into());
}

#[test]
fn test_return_in_init() {
    let mut out = Vec::new();
    let script = r#"
    class Foo {
        init() {
          print this;
        }
    }

    var foo = Foo();
    print foo.init();
    "#;

    interpret(script, &mut out).unwrap();
    assert_outputted(out, "Foo instance\nFoo instance\nFoo instance".into());
}