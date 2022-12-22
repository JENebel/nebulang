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

    let primes = 0;
    for(i, 0, 1000) {
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
    
# Syntax reference
## Basics
- Lexical scoping and static types.
- Everything returns a value.
- The last statement in a block (and in the program itself), is the return value.
- All statements need a semicolon after it, except the last line in a block (and in the program itself)
- Blocks do not need a semicolon after it, this includes if, while, functions etc.
- Declarations (fun and let expsression) and assignments all return unit
- Variable and function names must start with a letter or '_'

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

## Comments
Rest-of-line comments with // <br>
Block comments with /* */
<br>

    //This is a single line comment

    /* This
    is a
    block 
    comment */

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
There are several ways to make a for loop.
<br>
The simplest is just repeating something N times, where N is an int:

    for(N) {
        //Repeats N times
    }

Often we need a more flexible for loop. The general syntax is as follows:

    for(ID, FROM, TO, BY) {
        //Body
    }

FROM and TO can be both float and int, but beware of type mixing. <br>
If FROM is float, TO and BY can be either float or int or mixed, but if FROM is int, TO and BY must be int as well. <br>
BY can be left out, and it will default to 1. So these are equivalent:

    for(i, 0, 10, 1) {
        // Repeated 10 times
    }
    for(i, 0, 10) {
        // Repeated 10 times
    }

With floats:

    for(i, 0.0, 10, 0.5) {
        // Repeats 20 times
         // i will take these values:
         // i = 0.0
         // i = 0.5
         // ...
         // i = 9.5
    }

<br>
The loop variable will move from FROM to TO by BY. <br>
If TO is smaller than FROM, it will just move from highest to lowest. <br><br>
If TO > FROM then TO will be exclusive. <br>
If FROM > TO then FROM will be exlcusice. <br>
If equal the loop will not run at all. <br>
This is to make it as painless as possible to iterate arrays <br><br>

Here is a visualization:

    for(i, 0, 10, 1) {
        // i will take these values:
        // i = 0
        // i = 1
        // ...
        // i = 9
    }

    for(i, 10, 0, 1) {
        // i will take these values:
        // i = 9
        // i = 8
        // ...
        // i = 0
    }