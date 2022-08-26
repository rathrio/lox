use super::*;
use crate::ast;

fn parse(input: &str) -> Result<Program> {
    Parser::parse_str(input)
}

fn sexp(input: &str) -> String {
    let program = parse(input).unwrap();
    ast::sexp(&program)
}

fn parse_expr(input: &str) -> Result<Expr> {
    Parser::parse_expr_str(input)
}

fn sexp_expr(input: &str) -> String {
    let expr = parse_expr(input).unwrap();
    ast::sexp_expr(&expr)
}

#[test]
fn test_literals() {
    assert_eq!("42", sexp_expr("42"));
    assert_eq!("\"spongebob\"", sexp_expr("\"spongebob\""));
    assert_eq!("true", sexp_expr("true"));
    assert_eq!("false", sexp_expr("false"));
    assert_eq!("nil", sexp_expr("nil"));
}

#[test]
fn test_unary() {
    assert_eq!("(! false)", sexp_expr("!false"));
}

#[test]
fn test_unary_precedence() {
    assert_eq!("(== (! false) (> 2 3))", sexp_expr("!false == 2 > 3"));
}

#[test]
fn test_binary_expr() {
    assert_eq!("(+ 1 2)", sexp_expr("1 + 2"));
    assert_eq!("(- (+ 1 2) 3)", sexp_expr("1 + 2 - 3"));
    assert_eq!("(<= 1 42)", sexp_expr("1 <= 42"));
    assert_eq!("(== 3 1)", sexp_expr("3 == 1"));
}

#[test]
fn test_grouping() {
    assert_eq!("(* (group (+ 1 2)) 3)", sexp_expr("(1 + 2) * 3"));
    assert_eq!("(group (group (group (+ 1 2))))", sexp_expr("(((1 + 2)))"));
}

#[test]
fn test_binary_expr_precedence() {
    assert_eq!("(+ 1 (* 2 3))", sexp_expr("1 + 2 * 3"));
    assert_eq!("(+ 1 (/ 2 3))", sexp_expr("1 + 2 / 3"));
    assert_eq!("(+ (+ 1 (* 2 3)) 4)", sexp_expr("1 + 2 * 3 + 4"));
    assert_eq!("(+ (/ 1 2) 3)", sexp_expr("1 / 2 + 3"));
    assert_eq!("(!= (> 1 3) (<= 23 2))", sexp_expr("1 > 3 != 23 <= 2"));
}

#[test]
fn test_comma_expr() {
    assert_eq!("(, (+ 1 3) (+ 2 (/ 3 2)))", sexp_expr("1 + 3, 2 + 3 / 2"));
    assert_eq!("(, (, 1 2) 3)", sexp_expr("1, 2, 3"));
    assert_eq!("(, (, (, 1 2) 3) (! 42))", sexp_expr("1, 2, 3, !42"));
}

#[test]
fn test_ternary_expr() {
    assert_eq!(
        "(? (> (+ 3 2) 1) (/ 4 2) 42)",
        sexp_expr("3 + 2 > 1 ? 4 / 2 : 42")
    );
}

#[test]
fn test_ternary_expr_precedence() {
    assert_eq!("(? 1 2 (? 3 4 5))", sexp_expr("1 ? 2 : 3 ? 4 : 5"));
}

#[test]
fn test_expr_errors() {
    assert!(parse_expr("1 +").is_err());
    assert!(parse_expr("(42").is_err());
}

#[test]
fn test_binary_lhs_missing_error() {
    match parse_expr(" / 34") {
        Ok(_) => panic!("this parse should not succeed"),
        Err(e) => assert!(e.report.contains("LHS")),
    };
}

#[test]
fn test_program() {
    assert_eq!("(print \"Hello World!\")", sexp("print \"Hello World!\";"));

    assert_eq!(
        "(+ 1 2) (print \"Hello World!\")",
        sexp("1 + 2; print \"Hello World!\";")
    );
}

#[test]
fn test_var_decl() {
    assert_eq!("(var a nil)", sexp("var a;"));
    assert_eq!("(var a 42)", sexp("var a = 42;"));
    assert_eq!(
        "(var name (+ \"hi \" \"tadeus\"))",
        sexp("var name = \"hi \" + \"tadeus\";")
    );
}

#[test]
fn test_assignment() {
    assert_eq!("(var a nil) (= a 42)", sexp("var a; a = 42;"));
    assert!(parse_expr("1 + 2 = 42").is_err());
}

#[test]
fn test_block() {
    assert_eq!(
        "(block (var a 42) (print a))",
        sexp("{ var a = 42; print a; }")
    );
}

#[test]
fn test_if_else() {
    let script = r#"
        if (1 + 1)
            print "hey";
        "#;
    assert_eq!("(if (+ 1 1) (print \"hey\"))", sexp(script));

    let script = r#"
        if (42)
            print "hey";
        else
            print "ho";
        "#;
    assert_eq!("(if 42 (print \"hey\") (print \"ho\"))", sexp(script));
}

#[test]
fn test_logical_expr() {
    assert_eq!("(or 42 (+ 4 2))", sexp_expr("42 or 4 + 2"));
}

#[test]
fn test_while() {
    let script = r#"
        while (true)
            print "y";
        "#;
    assert_eq!("(while true (print \"y\"))", sexp(script));

    let script = r#"
        while (true) {
            print "y";
        }
        "#;
    assert_eq!("(while true (block (print \"y\")))", sexp(script));
}

#[test]
fn test_for() {
    let script = r#"
        for (var i = 0; i < 10; i = i + 1)
            print i;
        "#;
    assert_eq!(
        "(block (var i 0) (while (< i 10) (block (block (print i)) (= i (+ i 1)))))",
        sexp(script)
    );

    let script = r#"
        for (;;) {
            print "y";
        }
        "#;
    assert_eq!("(block (while true (block (print \"y\"))))", sexp(script));
}

#[test]
fn test_break() {
    let script = r#"
        while (true) break;
        "#;
    assert_eq!("(while true break)", sexp(script));

    assert!(parse("break;").is_err());
    assert!(parse("if (1) break;").is_err())
}

#[test]
fn test_call() {
    assert_eq!(
        "(fun add (a b c) ()) (call add (1 2 (! 42)))",
        sexp("fun add(a, b, c) {} add(1, 2, !42);")
    );
}

#[test]
fn test_call_precedence() {
    let script = r#"
    class Bar {}
    print !Bar;
    print !Bar();
    "#;

    assert_eq!(
        "(class Bar () () ()) (print (! Bar)) (print (! (call Bar ())))",
        sexp(script)
    );
}

#[test]
fn test_fun_decl() {
    let script = r#"
        fun add(a, b) {
            b = b + 1;
            print a + b;
        }
        "#;
    assert_eq!(
        "(fun add (a b) ((= b (+ b 1)) (print (+ a b))))",
        sexp(script)
    );
}

#[test]
fn test_return() {
    let script = r#"
        fun add(a, b) {
            return a + b;
        }
        "#;
    assert_eq!("(fun add (a b) ((return (+ a b))))", sexp(script));
}

#[test]
fn test_anon_fun() {
    assert_eq!(
        "(fun (x y) ((return (+ x y))))",
        sexp_expr("fun (x, y) { return x + y; }")
    );
}

#[test]
fn test_anon_functions_as_expr_stmt() {
    let script = "fun () {};";
    assert!(parse(script).is_err());
}

#[test]
fn test_naming_collision() {
    let script = r#"
        fun bad() {
            var a = "first";
            var a = "second";
        }
        "#;

    assert!(parse(script).is_err());
}

#[test]
fn test_return_outside_function() {
    let script = "return 42;";
    assert!(parse(script).is_err());

    let script = "{ return 42; }";
    assert!(parse(script).is_err());
}

#[test]
fn test_nested_return() {
    let script = r#"
        fun add() {
            while (true) {
                return;
            }
        }
        "#;

    parse(script).unwrap();
}

#[test]
fn test_classes() {
    assert_eq!("(class Dog () () ())", sexp("class Dog {}"));

    let script = r#"
        class Breakfast {
            cook() {
              print "Eggs a-fryin'!";
            }

            serve(who) {
              print "Enjoy your breakfast, " + who + ".";
            }
        }
        "#;

    assert_eq!(
        r#"(class Breakfast () () ((fun cook () ((print "Eggs a-fryin'!"))) (fun serve (who) ((print (+ (+ "Enjoy your breakfast, " who) "."))))))"#,
        sexp(script)
    );
}

#[test]
fn test_get() {
    assert_eq!(
        "(. someObject someProperty)",
        sexp_expr("someObject.someProperty")
    );

    assert_eq!(
        "(call (. (call (. egg scramble) (3)) with) (cheddar))",
        sexp_expr("egg.scramble(3).with(cheddar)")
    );
}

#[test]
fn test_set() {
    assert_eq!(
        "(.= (. (. breakfast omelette) filling) meat ham)",
        sexp_expr("breakfast.omelette.filling.meat = ham")
    );
}

#[test]
fn test_this_is_only_valid_in_methods() {
    let script = r#"
    print this;
    "#;
    assert!(parse(script).is_err());

    let script = r#"
    fun notAMethod() {
        print this;
    }
    "#;
    assert!(parse(script).is_err());
}

#[test]
fn test_returning_value_from_init_is_invalid() {
    let script = r#"
    class Foo {
        init() {
            return "something else";
        }
    }
    "#;
    assert!(parse(script).is_err());

    let script = r#"
    class Foo {
        init() {
            return;
        }
    }
    "#;
    assert!(parse(script).is_ok());
}

#[test]
fn test_class_methods() {
    let script = r#"
    class Math {
      class square(n) {
        return n * n;
      }
    }
    "#;

    assert_eq!(
        "(class Math () ((fun square (n) ((return (* n n))))) ())",
        sexp(script)
    );
}

#[test]
fn test_inheritance() {
    let script = r#"
    class Doughnut {
        // General doughnut stuff...
    }

    class BostonCream < Doughnut {
      // Boston Cream-specific stuff...
    }
    "#;

    assert_eq!(
        "(class Doughnut () () ()) (class BostonCream (Doughnut) () ())",
        sexp(script)
    );

    let script = "class Oops < Oops {}";
    assert!(parse(script).is_err());
}

#[test]
fn test_invalid_supers() {
    let script = r#"
    class Eclair {
      cook() {
        super.cook();
        print "Pipe full of crème pâtissière.";
      }
    }
    "#;
    assert!(parse(script).is_err());

    assert!(parse_expr("super.notEvenInAClass()").is_err());
}
