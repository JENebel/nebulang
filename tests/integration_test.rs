mod common;

use nebulang::runner::*;
use nebulang::ast::Literal;
use common::*;

#[test]
fn plus() {
    let input = "
        2+3
    ";
    
    let result = run_program(input.to_string(), Vec::new());

    assert!(expect(result, Literal::Int(5)))
}

#[test]
fn recursion() {
    let input = "
        fun f(a: int, b: int): int = 
            if(a == 0) b 
            else f(a-1, b + 1);
        f(2*12, 22+5)
    ";
    
    let result = run_program(input.to_string(), Vec::new());

    assert!(expect(result, Literal::Int(51)))
}

#[test]
fn primes() {
    let input = "
        let primes = 0;
        for(i, 0, 2500) {
            let p = true;
            let d = 2;
            if(i <= 1) p = false;
            while (d<i-1 && p) {
                if(i%d==0) p=false;
                d+=1
            }
            if(p) primes += 1;
        }
        primes
    ";
    
    let result = run_program(input.to_string(), Vec::new());

    assert!(expect(result, Literal::Int(367)))
}