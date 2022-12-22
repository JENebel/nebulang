# Nebulang
Small interpreted language with static types and lexical scoping implemented in Rust.

<br>
Here are some examples:

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

    let i = 0;
    let primes = 0;
    while(i < 1000) {
        let p = true;
        let d = 2;
        let go = true;
        if(i <= 1) p = false;
        while (d<i-1 && p) {
            if(i%d == 0) p = false;
            d+=1
        }
        if(p) primes += 1;
        i+=1
    }
    primes
    
# Syntax reference
## Basics
- Lexical scoping and static types.
- Everything returns a value.
- The last statement in a block (and in the program itself), is the return value.
- All statements need a semicolon after it, except the last line in a block (and in the program itself)
- Blocks do not need a semicolon after it, this includes if, while, functions etc.
- Declarations (fun, let exps) return unit
- variable and function names must start with a letter or '_'

<br>

There are the following keywords:
<br>

    let, if, else, while, for, fun
      
There are the following types:
<br>

    int, float, bool, unit

There are the following operators:
<br>

    +=, -=, +, -, *, /, %, <=, >=, <, >, !=, !, ==, =, &&, ||

## Let
    let foo = 12
    let bar = false

## Functions
Functions can be declared anywhere in a block, and it will always be available in that scope. That means that the following will work fine:

    foo(7);
    fun foo(a: int) = a * 2
    //Returns 14

This also ensures both self-recursion and mutual recursion. A function can acces all variables declared before the declaration of the function AND the calling location.

The example also illustrates that there is no need for a block if the function is a single statement.

<br>
Type annotations are needed for function parameters, but it is optional for the return type.
<br> The type checker might not be able to infer the return type, and in that case it will give an error.

Type annotations for return types look like this:

    fun foo(a: int): int = a * 2
## If
Blocks in the cases are optional, so

    if (n < 0) 0 else n
    
is equivalent to

    if (n < 0) {0} else {n}

<br>
They can easily be chained

    if (n < 0) 0
    else if (n > 25) 25
    else n
## While
A while loop always returns unit.
<br>
Here is an example of a while loop:

    let i = 0;
    while(i < 10) {
        i++
    }
## For
For is not implemented yet
