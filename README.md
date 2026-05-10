# Yafl – Yet Another Functional Language

A toy implementation of a tiny functional programming language.

## Summary

Yafl is essentially an implementation of [System F](https://en.wikipedia.org/wiki/System_F) with a few extensions.
The following illustrates a simple program:

```yafl
((x: Int) => x + 1) 2
```

Yafl programs are compiled to WebAssembly, in either textual or binary format.
For example:

```
% echo "3 + 3" >> example.yafl
% sbt "run --wat -o - example.yafl"
(module (memory $__m 1)
;; ...
(func (export "main") (result i32) (i32.const 6)))
```

The compiler accepts the following options to control its output:

- `-o` : Specifies the file in which the compiler writes its output.
  You may use `-o -` to have the compiler write its result to the standard output instead.
- `--syntax` : Configures the compiler to only parse the input file.
- `--wat` : Configures the compiler to generate textual assembly.
- `--wasm` : Configures the compiler to generate binary assembly (default).

## Features

As a pure functional programming language, Yafl naturally supports first-class functions.
For example, the following program illustrates the application of the identity.

```yafl
((x : Int) => x) 1
```

The function (or lambda) `(x : Int) => x` denotes the identity, i.e., the function that simply returns its argument, and it is applied to `1`, which is an integer literal.

In addition to user functions, Yafl also supports basic integer arithmetic operations and comparisons.
The following program illustrates with a function computing the absolute value of its argument:

```yafl
(x : Int) => if x < 0 then -x else x
```

One can define local bindings to split long expressions or avoid repetition:

```yafl
let min = (x : Int) => (y : Int) => if y < x then y else x ;
min 3 4
```

Yafl also supports generic definitions.
For example, the following program illustrates the application of the *polymorphic* identity:

```yafl
([T] => (x : T) => x) [Int] 1
```

The function `(x : T) => x` is defined under an abstraction introducing a type parameter `T`.
This parameter is instantiated with `Int`, resulting in a function of type `Int -> Int` that is finally applied to the literal `1`.

Recursion can be expressed using a fixed-point combinator:

```yafl
let factorial = fix loop : Int -> Int =
  (n : Int) =>
    if n < 2 then 1 else n * (factorial (n - 1)) ;
factorial 6
```

The execution environment of a Yafl program can configure a number of command-line arguments, provided in the form of an array of integers.
Two built-in constructs can be used from Yafl programs to interact with these arguments.
The first, `#argc`, denotes the number of arguments available (i.e., the length of the array).
The second, `#argv`, denotes a function returning the `i`-th argument.
For example, the following program sums all command-line arguments.

```yafl
let sum = fix loop : Int -> Int -> Int =
  (i : Int) => (a : Int) =>
    if i < #argc then loop (i + 1) (a + (#argv i)) else a ;
sum 0 0
```

## Syntax

The syntax of Yafl is described by the production rules below.
The following is assumed:

- Integer literals are contiguous sequences of digits (e.g., `123`); and
- Identifiers are strings of alphanumeric characters and the underscore, starting with a non-numeric character (e.g., `foo` or `_23`).

Whitespaces and comments (i.e., substrings prefixed by `//` and terminating at the next newline sequence) are simply ignored during lexing and are therefore irrelevant during parsing.

```
term ::=
  | term ['*' | '/'] add-term
  | add-term

add-term ::=
  | add-term ['+' | '-'] cmp-term
  | cmp-term

cmp-term ::=
  | cmp-term ['==' | '!=' | '<' | '<=' | '>=' | '>']  and-term
  | and-term

and-term ::=
  | and-term '&&' or-term
  | or-term

or-term ::=
  | or-term '&&' term-application
  | prefix-term

prefix-term ::=
  | ['!' | '-'] prefix-term
  | type-application

term-application ::=
  | type-application type-application?

type-application ::=
  | type-application '[' type (',' type)* ']'
  | simple-term

simple-term ::=
  | unit-literal
  | boolean-literal
  | integer-literal
  | identifier
  | builtin-identifier
  | term-abstraction
  | type-abstraction
  | conditional
  | binding
  | '(' term ')'

unit-literal ::=
  | '(' ')'

boolean-literal ::=
  | 'true'
  | 'false'

builtin-identifier ::
  | '#argc'
  | '#argv'

term-abstraction ::=
  | '(' identifier ':' type (',' identifier ':' type)* ')' '=>' term
  | 'fix' identifier ':' type '=' term

type-abstraction ::=
  | '[' identifier (',' identifier)* ']' '=>' term

conditional ::=
  | 'if' term 'then' term 'else' term

binding ::=
  | 'let' identifier '=' term ';' term

type ::=
  | arrow-type

arrow-type ::=
  | simple-type ('->' type)?

simple-type ::=
  | identifier
  | forall-type
  | '(' type ')'

forall-type
  | '[' identifier (',' identifier)* ']' => type
```

Note that the grammar has been designed to satisfy a couple of key properties:

1. It is unambiguous: there is at most one way to recognize a term of Yafl.
2. Is is [LL(k)](https://en.wikipedia.org/wiki/LL_parser): it is possible to construct a top-down parser for Yafl using *k* tokens of lookahead.

Thanks to these properties, we can parse Yafl programs left to right, applying the left-most derivation.
For example, consider the term:

```yafl
1 + - 2 * 3
```

A LL parser will perform the following derivation.
The non-terminal symbol being expanded is written in angle brackets in each line and parentheses have been added for clarity.
Trivial steps have been omitted for concision.

```
[term]
→ [term] '*' add-term
→ ([add-term] '+' cmp-term) '*' add-term
→ (integer-literal '+' [cmp-term]) '*' add-term
→ (integer-literal '+' [prefix-term]) '*' add-term
→ (integer-literal '+' ('-' [prefix-term])) '*' add-term
→ (integer-literal '+' ('-' integer-literal)) '*' [add-term]
→ (integer-literal '+' ('-' integer-literal)) '*' integer-literal
```

The resulting abstract syntax tree will correspond to the more explicit input string `(1 + (-2)) * 3`.
