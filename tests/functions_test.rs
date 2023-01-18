mod common;

use common::*;
use nebulang::ast::Literal::*;
use nebulang::ast::ErrorType::*;

#[test]
fn simple_fun() {
    let input = "
        fun foo(): int = 5;
        foo()
    ";

    expect_lit(input, Int(5));
}

#[test]
fn simple_return_type_inference() {
    let input = "
        fun foo() = 5;
        foo()
    ";

    expect_lit(input, Int(5));
}

#[test]
fn call_other_function_in_function() {
    let input = "
        fun foo() = 5;
        fun bar() = foo();
        bar()
    ";

    expect_lit(input, Int(5));
}

#[test]
fn use_var_from_outside_of_fun() {
    let input = "
        let x = 5;
        fun foo() = x;
        foo()
    ";

    expect_lit(input, Int(5));
}

#[test]
fn simple_recursion() {
    let input = "
        fun even(x: int): bool =
            if (x == 0) true else odd(x-1);

        fun odd(x: int): bool =
            if (x == 0) false else even(x-1);
        
        even(10)
    ";

    expect_lit(input, Bool(true));
}

#[test]
fn recursive_functions_can_not_acces_vars_declared_between() {
    let input = "
        fun even(x: int): bool =
            if (x == 0) true else odd(x-1);

        let b = false;

        fun odd(x: int): bool =
            if (x == 0) b else even(x-1);
        
        even(10)
    ";

    expect_err(input, TypeError);
}

#[test]
fn recursive_functions_can_acces_vars_declared_before_both() {
    let input = "
        let b = true;

        fun even(x: int): bool =
            if (x == 0) b else odd(x-1);

        fun odd(x: int): bool =
            if (x == 0) !b else even(x-1);
        
        even(10)
    ";

    expect_lit(input, Bool(true));
}

#[test]
fn require_return_type_on_calling_recursive_function() {
    let input = "
        fun even(x: int) =
            if (x == 0) true else odd(x-1);

        fun odd(x: int): bool =
            if (x == 0) false else even(x-1);
        
        even(10)
    ";

    expect_err(input, TypeError);
}

#[test]
fn require_return_type_on_called_recursive_function() {
    let input = "
        fun even(x: int): bool =
            if (x == 0) true else odd(x-1);

        fun odd(x: int) =
            if (x == 0) false else even(x-1);
        
        even(10)
    ";

    expect_err(input, TypeError);
}

#[test]
fn cannot_call_function_outside_of_the_block_it_is_defined_in() {
    let input = "
        {
            fun foo() = 5
        }
        foo()
    ";

    expect_err(input, TypeError);
}

#[test]
fn simple_early_return() {
    let input = "
        fun foo(): int = {
            return 5;

            let x = 10;
            x
        }

        foo()
    ";

    expect_lit(input, Int(5));
}

#[test]
fn can_infer_return_type_from_return_statement() {
    let input = "
        fun foo() = {
            return 5;

            let x = 10;
            x
        }

        foo()
    ";

    expect_lit(input, Int(5));
}

#[test]
fn mismatched_return_types_disallowed() {
    let input = "
        fun foo() = {
            return 5.5;

            let x = 10;
            x
        }

        foo()
    ";

    expect_err(input, TypeError);
}