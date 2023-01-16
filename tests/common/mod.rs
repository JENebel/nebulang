use nebulang::{ast::{Literal, ErrorType}, runner::*};

pub fn expect_lit(input: &'static str, expected: Literal) {
    let res = match run_program(input.to_owned(), Vec::new()) {
        Ok((lit, _)) => 
            if lit != expected {
                println!("Expected '{expected}', but got '{lit}'");
                false
            } else {
                true
            },
        Err(err) => {
            println!("Expected '{expected}', but got '{err}'");
            false
        },
    };

    assert!(res)
}

pub fn expect_err(input: &'static str, expected: ErrorType) {
    let res = match run_program(input.to_owned(), Vec::new()) {
        Ok((lit, _)) => {
            println!("Expected '{expected}', but got '{lit}'");
            false
        }
        Err(err) => {
            if err.error_type != expected {
                println!("Expected '{expected}', but got '{err}'");
                false
            } else {
                true
            }
        }
    };

    assert!(res)
}