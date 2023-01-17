mod common;

use common::*;
use nebulang::ast::Literal::*;
use nebulang::ast::*;
use nebulang::ast::ErrorType::*;

#[test]
fn simple_return() {
    expect_lit("2", Int(2))
} 

#[test]
fn simple_precedence() {
    expect_lit("2 + 3 * 4", Int(14))
}

#[test]
fn double_operator_is_illegal() {
    expect_err("2 + + 3 * 4", SyntaxError)
}

#[test]
fn double_exp_is_illegal() {
    expect_err("2 4 + 3", SyntaxError)
}

#[test]
fn divide_by_zero_should_give_error() {
    expect_err("10 / 0", RuntimeError)
}

#[test]
fn simple_let() {
    let input = "
        let x = 12;
        x
    ";

    expect_lit(input, Int(12))
}

#[test]
fn simple_var_usage() {
    let input = "
        let x = 12;
        let y = x + 24;
        y
    ";

    expect_lit(input, Int(36))
}

#[test]
fn block_var_should_get_disposed() {
    let input = "
        {
            let x = 12;
        }
        x
    ";

    expect_err(input, TypeError)
}

#[test]
fn block_var_should_be_seperate_to_outer_variable() {
    let input = "
        let x = 0;
        {
            let x = 12;
        }
        x
    ";

    expect_lit(input, Int(0))
}

#[test]
fn simple_if() {
    let input = "
        let x = 0;
        if(true) x = 10;
        x
    ";

    expect_lit(input, Int(10))
}

#[test]
fn simple_if_with_block() {
    let input = "
        let x = 0;
        if(true) {
            let y = 10;
            x = y
        }
        x
    ";

    expect_lit(input, Int(10))
}

#[test]
fn simple_if_neg() {
    let input = "
        let x = 0;
        if(false) x = 10;
        x
    ";

    expect_lit(input, Int(0))
}

#[test]
fn simple_if_else() {
    let input = "
        let x = 0;
        if(false) x = -10;
        else x = 10;
        x
    ";

    expect_lit(input, Int(10))
}

#[test]
fn chained_if_else() {
    let input = "
        let x = 0;
        if(false) x = -10
        else if (false) x = 10
        else x = 5;
        x
    ";

    expect_lit(input, Int(5))
}

#[test]
fn array_initialization() {
    let input = "
        let arr = [3 of 10];
        arr
    ";

    expect_type(input, Type::Array(Box::new(Type::Int)))
}

#[test]
fn simple_array_access() {
    let input = "
        let arr = [3 of 10];
        arr[1]
    ";

    expect_lit(input, Int(10))
}

#[test]
fn array2d_access() {
    let input = "
        let arr = [3 of [3 of \"Ding\"]];
        arr[1][2]
    ";

    expect_lit(input, Str("Ding".to_string()))
}

#[test]
fn access_init_array_directly_access() {
    let input = "
        [3 of \"Ding\"][1]
    ";

    expect_lit(input, Str("Ding".to_string()))
}

#[test]
fn function_with_array_argument() {
    let input = "
        fun get_index(arr: [int], i: int) = arr[i];

        get_index([4 of 10], 1)
    ";

    expect_lit(input, Int(10))
}

#[test]
fn array_is_copied_when_passed() {
    let input = "
        fun set_index(arr: [int], i: int) = arr[i] = 2;

        let arr = [4 of 10];

        set_index(arr, 1);

        arr[1]
    ";

    expect_lit(input, Int(10))
}