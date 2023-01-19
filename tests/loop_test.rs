mod common;

use common::*;
use nebulang::definitions::Literal::*;
use nebulang::definitions::ErrorType::*;

#[test]
pub fn simple_while() {
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
pub fn simplest_for() {
    let input = "
        let x = 0;
        for(10) x += 1;
        x
    ";

    expect_lit(input, Int(10))
}

#[test]
pub fn simple_for() {
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
pub fn standard_for() {
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
pub fn standard_for_no_braces() {
    let input = "
        let x = 0;
        for(i=0; i<10; i+=1) x += 1;
        x
    ";

    expect_lit(input, Int(10))
}

#[test]
pub fn inverse_standard_for() {
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
pub fn for_condition_must_be_bool() {
    let input = "
        let x = 0;
        for(i=10; 0; i-=1) {
            x += 1
        }
        x
    ";

    expect_err(input, TypeError)
}

#[test]
fn simple_break() {
    let input = "
        let x = 0;
        for(i=0; i<10; i+=1) {
            if(i == 5) break;
            x += 1
        }
        x
    ";

    expect_lit(input, Int(5));
}

#[test]
fn simple_continue() {
    let input = "
        let x = 0;
        for(i=0; i<10; i+=1) {
            if(i % 2 == 0) continue;
            x += 1
        }
        x
    ";

    expect_lit(input, Int(5));
}

#[test]
fn simple_loop() {
    let input = "
        let x = 0;
        loop {
            if (x == 12) break;
            x += 1
        }
        x
    ";

    expect_lit(input, Int(12));
}