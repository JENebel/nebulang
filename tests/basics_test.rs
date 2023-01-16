mod common;

use common::*;
use nebulang::ast::Literal::*;
use nebulang::ast::ErrorType::*;

#[test]
fn simple_return_test() {
    expect_lit("2", Int(2))
} 

#[test]
fn simple_precedence_test() {
    expect_lit("2 + 3 * 4", Int(14))
}

#[test]
fn divide_by_zero_should_give_error() {
    expect_err("10 / 0", RuntimeError)
}

#[test]
fn simple_let_test() {
    let input = "
        let x = 12;
        x
    ";

    expect_lit(input, Int(12))
}

#[test]
fn simple_var_usage_test() {
    let input = "
        let x = 12;
        let y = x + 24;
        y
    ";

    expect_lit(input, Int(36))
}