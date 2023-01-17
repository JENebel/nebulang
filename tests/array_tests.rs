mod common;

use common::*;
use nebulang::ast::Literal::*;
use nebulang::ast::ErrorType::*;
use nebulang::ast::Type;

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
fn can_modify_array_index() {
    let input = "
        let arr = [4 of 10];
        arr[2] = 2;
        arr[2]
    ";

    expect_lit(input, Int(2))
}

#[test]
fn can_modify_2d_array_index() {
    let input = "
        let arr = [4 of [4 of 10]];
        arr[2][2] = 2;
        arr[2][2]
    ";

    expect_lit(input, Int(2))
}

#[test]
fn array_usage_in_function() {
    let input = "
        fun doubled_index(arr: [int], index: int) = 2 * arr[index];
        let arr = [10 of 5];
        doubled_index(arr, 3)
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

#[test]
fn array_is_copied_when_assigned() {
    let input = "
        let arr = [4 of 10];
        let arr2 = arr;
        arr2[2] = 2;
        arr[2]
    ";

    expect_lit(input, Int(10))
}

#[test]
fn require_correct_type_in_array_index_assign() {
    let input = "
        let arr = [4 of 10];
        arr2[2] = \"2\";
        arr[2]
    ";

    expect_err(input, TypeError)
}

#[test]
fn use_array_element_in_expression() {
    let input = "
        let arr = [4 of 10];
        let res = 2 + arr[2];
        res
    ";

    expect_lit(input, Int(12))
}

#[test]
fn iterate_array() {
    let input = "
        let arr = [4 of 10];
        let res = 0;
        for(i = 0; i < 4; i += 1) {
            res += arr[i];
        }
        res
    ";

    expect_lit(input, Int(40))
}

#[test]
fn iterate_2d_array() {
    let input = "
        let arr = [5 of [10 of 1]];
        let res = 0;
        for(i = 0; i < 5; i += 1) {
            for(y = 0; y < 10; y += 1) {
                res += arr[i][y];
            }
        }
        res
    ";

    expect_lit(input, Int(50))
}

#[test]
fn can_not_access_operator() {
    let input = "
        -[3];
    ";

    expect_err(input, SyntaxError)
}

#[test]
fn initialize_simple_array_with_given_values() {
    let input = "
        [1, 2, 3, 4]
    ";

    expect_type(input, Type::Array(Box::new(Type::Int)))
}

#[test]
fn initialize_array_from_expressions() {
    let input = "
        let x = 12;
        let arr = [
            x, 
            x + 1, 
            {
                x = 2;
                x
            }, 
            x * 4
        ];
        arr[0] + arr[1] + arr[2] + arr[3]
    ";

    expect_lit(input, Int(35))
}

#[test]
fn require_same_type_in_array_values() {
    let input = "
        let arr = [12, 23, \"monkey\"];
        arr[2]
    ";

    expect_err(input, TypeError)
}

#[test]
fn cannot_have_unit_in_array() {
    let input = "
        let arr = [{}, {}, {}];
        arr[2]
    ";

    expect_err(input, TypeError)
}