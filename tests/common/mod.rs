use nebulang::{ast::Literal, runner::RunStats};

pub fn expect(found: Result<(Literal, RunStats), String>, expected: Literal) -> bool {
    if let Ok((lit, _)) = found {
        lit == expected
    } else {
        false
    }
}