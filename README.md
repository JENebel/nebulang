# Nebulang

Small interpreted language with static types and lexical scoping implemented in Rust.

- [Nebulang](#nebulang)
  - [Examples](#examples)
  - [Running a program](#running-a-program)
    - [Arguments](#arguments)
  - [Basics](#basics)
    - [Keywords](#keywords)
    - [Types](#types)
    - [Operators](#operators)
    - [Comments](#comments)
  - [Syntax reference](#syntax-reference)
    - [Let](#let)
    - [If](#if)
    - [Functions](#functions)
    - [Arrays](#arrays)
    - [While](#while)
    - [For](#for)

## Examples

Simple add function:

    fun add(a: int, b: int) = a + b;
    add(12, 5)

Simple factorial function:

    fun fact(n: int) = {
        fun f(n: int, acc: int): int = {
            if (n == 0) acc else {
                f(n - 1, n * acc)
            }
        }

        f(n, 1);
    }
    fact(5)

Counts number of primes up to 1000:

    let primes = 0;
    for(i = 0; i <= 1000; i += 1) {
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

## Running a program

To run one of the example programs use

    cargo run --release examples/primes

Both relative and absolute paths work.

If the **.nbl** extension is used, there is no need for the extension when running.

### Arguments

    --info      // Shows extra info about the execution

## Basics

- Lexical scoping and static types.
- Everything returns a value.
- The last statement in a block (and in the program itself), is its return value.
- All statements need a semicolon after it, except the last in a block (and in the program itself) where it is optional
- Blocks do not need a semicolon after it, this includes usage in if, while, functions etc.
- Declarations, assignments, loops, etc. all return unit
- Variable and function names must start with a letter or '_'
- There is no 'null' or similar, all variables always have legal values
- Variable types are static and automatically inferred from its initial value

### Keywords

    let, if, else, while, for, fun, of

### Types

    int, float, bool, char, string, unit, arrays

They are assigned like this.

    let i = 420;     // int
    let f = 3.141;   // float
    let b = true     // bool
    let c = 'c'      // char
    let s = "string" // string. Quotes can be used with \"

\+ operation with a string as one side simply concatenates into a string.\
Also char + char => string.

### Operators

    +=, -=, +, -, *, /, %, <=, >=, <, >, !=, !, ==, =, &&, ||

### Comments

Rest-of-line comments with //\
Block comments with /**/

    // This is a single line comment

    /* This
    is a
    block 
    comment */

An unclosed block comment comments out the rest of the file.

## Syntax reference

### Let

    let foo = 12
    let bar = false

### If

Blocks in the cases are optional, so these are equivalent:

    if (n < 0) 0 else n;
    if (n < 0) {0} else {n}
\
They can also easily be chained:

    if (n < 0) 0
    else if (n > 25) 25
    else n

### Functions

The simplest use of a function looks like this:

    fun add(a: int, b: int) = a + b;
    add(5, 10)
    // Returns 15

Functions can be declared anywhere, and it will always be available in that scope.\
This also ensures both self-recursion and mutual recursion.

A function can acces all variables declared before the declaration of the function *and* the first calling location.

As mentioned functions can be called before it is declared, but in that case the function needs a type annotation. This is to enable type checks in recursive functions.

Here is an example of mutually recursive functions that access the variable 'g':

    let g = 0;
    bar(10);
    fun bar(n: int): unit = {
        g += 1;
        if(n>0) baz(n);
    }
    fun baz(n: int): unit = {
        g += n;
        if(n>0) bar(n-1);
    }
    g

Type annotations are needed for function parameters, but it is optional for the return type.\
The type checker might not be able to infer the return type, and in that case it will give an error.

Type annotations for return types look like this:

    fun foo(a: int): int = a * 2

### Arrays

Arrays are initialized as follows:

    let arr = [12 of "A"];

This will result in an array of length 12. The array will have the type [string], which means 'an array of strings'.\
The "A" is the 'template' for the array, which is the value the elements will have initially. The template can not be a reference.

Arrays are accesed like this:

    let arr = [12 of "A"];
    arr[5]
    // Returns "A"

Multi-dimensional arrays are simply used like this: 

    let arr = [12 of [12 of "A"]];
    arr[5][3]
    // Returns "A"

An array type is written like this:

    [int]       // 1 dimension
    [[int]]     // 2 dimensions

An example use in a function:

    fun doubled_index(arr: [int], index: int) = 2 * arr[index];
    let arr = [10 of 5];
    doubled_index(arr, 3)
    // Returns 10

### While

A while loop always returns unit.\
Here is an example of a while loop:

    let i = 0;
    while(i < 10) {
        i++
    }

### For

There are 2 ways to make a for loop.\
The simplest is just repeating something n times, where n is an int:

    for([n]) {
        // Repeats n times
    }

Often we need a more flexible for loop. The general syntax is as follows:

    for([id] = [from]; [condition]; [increment]) [body]

In practice:

    for(i = 0; i < 10; i += 1) {
        // Repeated 10 times
    }

Condition must evaluate to a boolean, but increment can be any expression. Body does not need to be surrounded by brackets if it is a single statement/expression.
