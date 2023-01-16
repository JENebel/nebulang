mod common;

use common::*;
use nebulang::ast::Literal::*;
use nebulang::ast::ErrorType::*;

#[test]
fn recursion() {
    let input = "
        fun f(a: int, b: int): int = 
            if(a == 0) b 
            else f(a-1, b + 1);
        f(2*12, 22+5)
    ";

    expect_lit(input, Int(51));
}

#[test]
fn lort() {
    let input = "
        fun f(a: int, b: int): int = 
            if(a == 0) b 
            else f(a-1, b + 1);
        f(2*12, 22'*+5)
    ";

    expect_err(input, SyntaxError);
}