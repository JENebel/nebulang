mod common;

use common::*;
use nebulang::ast::Literal::*;
use nebulang::ast::ErrorType::*;

#[test]
pub fn while_test() {
    let input = "
        let x = 0;
        let i = 0;
        while(i < 10) {
            x += 1;
            i += 1;
        }
        x
    ";

    expect_lit(input, Int(10))
}

#[test]
pub fn simplest_for_test() {
    let input = "
        let x = 0;
        for(10) x += 1;
        x
    ";

    expect_lit(input, Int(10))
}

#[test]
pub fn simple_for_test() {
    let input = "
        let x = 0;
        for(10) {
            x += 1
        }
        x
    ";

    expect_lit(input, Int(10))
}

#[test]
pub fn standard_for_test() {
    let input = "
        let x = 0;
        for(i=0; i<10; i+=1) {
            x += 1
        }
        x
    ";

    expect_lit(input, Int(10))
}

#[test]
pub fn standard_for_no_braces_test() {
    let input = "
        let x = 0;
        for(i=0; i<10; i+=1) x += 1;
        x
    ";

    expect_lit(input, Int(10))
}

#[test]
pub fn inverse_standard_for_test() {
    let input = "
        let x = 0;
        for(i=10; i>0; i-=1) {
            x += 1
        }
        x
    ";

    expect_lit(input, Int(10))
}

#[test]
pub fn for_condition_must_be_bool_test() {
    let input = "
        let x = 0;
        for(i=10; 0; i-=1) {
            x += 1
        }
        x
    ";

    expect_err(input, TypeError)
}