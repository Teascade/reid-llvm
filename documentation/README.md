# Reid Language

This is where the documentation for this language will go, describing the
features, syntax and standard library of the language as best as I have time and
motivation to write.

Documentation is presented in a formal grammar and an example,
syntax-highlighted with Rust, because it's close enough.

## Syntax

Syntax for Reid is very much inspired by rust, and examples of the language can
be found in the [examples](../examples/)-folder.

In Reid **modules** (or files) on the top-level are comprised of imports, type
definitions, binop-definitions and functions.

In formal grammar
```bnf
<module> :: (<import> | <type-definition> | <binop-definition> | <function>)*
```

Table of Contents:
- [Common tokens](#common-tokens)
- [Imports](#imports)
- [Type definitions](#type-definitions)
    - [Struct types](#struct-types)
- [Binary operation Definitions](#binary-operation-definitions)
- [Function definitions](#function-definition)
- [Statement](#statement)
- [Expression](#expression)

### Common tokens

Common token used throughout this document to express parts of grammar include:
```bnf
<ident> :: [a-Z]([_a-Z0-9])*

<literal> :: <integer> | <real> | <char> | <string> | <bool>
<integer> :: <decimal> | <hexadecimal> | <octal> | <binary>
<real> :: [0-9]+ "." [0-9]+
<char> :: "'" <any-character> "'"
<string> :: "\"" <any-character>* "\""
<bool> :: "true" | "false"

// Any character (except "), or any character escaped
<any-character> :: [.] | "\" [.]

<decimal> :: [0-9]+
<hexadecimal> :: "0x" [0-9a-f]+
<octal> :: "0o" [0-7]+
<binary> :: "0b" [01]+


<type> :: <primitive-type>
    | "[" <type> ";" <integer>] "]"
    | "*" <type>
    | "&" [ "mut" ] <type>

<primitive-type> :: 
    "char" | "bool" |
    "u8" | "u16" | "u32" | "u64" | "u128" |
    "i8" | "i16" | "i32" | "i64" | "i128" |
    "f16" | "f32" | "f32b" | "f64" | "f80" | "f128" | "f128ppc"

<binop> :: "+" | "-" | "*" | "/" | "%" | "&&" | <cmp>
<cmp> :: "<" | "<=" | "==" | "!=" | ">=" | >"
<unary> :: "+" | "-" | "!"
```

### Imports

Imports are used to import functions and types from other modules. In formal
grammar the syntax for imports is
```bnf
<import> :: "import" <ident> "::" <ident> ";"
```

An example importing the `print`-function from `std` would be
``` rust
import std::print;
```

### Type Definitions

Type definitions are used to define new types. Currently this only supports
struct-types. In formal grammar:
```bnf
<type-definition> :: <struct-definition>
```

#### Struct types

Struct (or Structure) types are aggregate types containing other types within
struct fields. In formal grammar:
```bnf
<struct-definition> :: "struct" <ident> "{" [ <field-def> ( "," <field-def> )* [ "," ] ] "}"
<field-def> :: <ident> ":" <type>
```

An example of a struct `Test` containing two fields `first` and `second` of
integer types.
```rust
struct Test {
    first: u32,
    second: u64,
}
```

### Binary Operation Definitions

Reid has a feature where custom binary operations can be defined with a
specialized syntas. Only pre-defined operators are allowed however. In formal
grammar:

```bnf
<binop-definition>: "impl" "binop" <param> <binop> <param> "-> " <type> <block>
<param> :: "(" <ident> ":" <type> ")"
```

(Block-syntax is defined formally with functions)

An example of a custom binary operator `+` between type `u16` and `u32` could be:
```rust
impl binop (lhs: u16) + (rhs: u32) -> u32 {
    return (lhs as u32) + rhs;
}
```

### Function Definition

Rust syntax for defining functions is similar to rust. There are two types of functions:
1. `extern` functions which are defined in another module, used to define functions from outside modules such as `libc`.
2. `local` functions which are defined locally in the module in Reid. Their
   definition is contained within a `block` which contains a list of
   `statement`s.

In formal grammar:
```bnf
<function-definition> :: <extern-function> | <local-function>
<extern-function> :: "extern" "fn" <signature> ";"
<local-function> :: [ "pub" ] "fn" <signature> <block>

<signature> :: <ident> "(" [ <params> ] ")" [ "->" <type> ]
<params> <param> ( "," <param> )*
<param> :: <ident> ":" <type>
<block> :: "{" <statement>* "}"
```

An example of a simple `extern` and `local` function definition would be:
```rust
extern fn puts(message: *char) -> i32;

fn main() -> u8 {
    return 7;
}
```

### Statement

Statements in Reid is how you tell the program to do anything. Currently supported statements include:
- Let-statement to declare new variables
  - Let-statements declare immutable variables by-default, but can declare
    mutable variables with the `mut` keyword similar to Rust.
- Set-statement to re-set previously declared (mutable) variables to new values.
- Statements can also be simply expressions without intent to store the result.
- Return-statements to return values out of blocks or functions.
- For-loops to loop over a certain range of numbers
- While-loops to loop until a certain condition is no longer met.

In formal grammar:
```bnf
<statement> :: <let> | <set> | <return> | <for> | <while> | <expr-statement>
<let> :: "let" [ "mut" ] <ident> "=" <expression> ";"
<set> :: <ident> "=" <expression> ";"
<expr-statement> :: <expression> ";"
<for> :: "for" <ident> "in" <expression> ".." <expression> <block>
<while> :: "while" <expression> <block>
<return> :: ( "return" <expression> ";" ) | <expression>
```

An example of each statement type would be:
```rust
let mut value = 5;
value = 6;
for i in 0..5 { }
while value < 5 { }

// "hard" return
return value; 

// "soft" return
value
```

### Expression

Expressions in Reid are anything that can return a value, such as function
calls, literals, or if-expressions. Types of supported expressions include:
- **Variable name reference**, to reference a value in a variable
- **Borrow**, which works similar to Rust but with a little less safety. In simple
  terms it creates a safer pointer-type than a regular pointer.
- **Deref**, which extracts the value out of a borrow.
- **Literal**, which is just some direct value, such as a number.
- **Array-value**, to declare a new array with a static length
- **Struct-value**, to declare a new value for a struct that has been defined
  earlier.
- Shorter way to declare arrays with a single initialized value.
- **Indexing** into an array-value
- **Accessing a field** in a struct-value
- **Binary operations** (such as add/sub/mult)
- **Unary operations** (such as !value or -value)
- **Function calls**, to invoke a predefined function with given parameters
- **Block-expressions**, which can return a value to the higher-level expression
  if they have a statement with a soft-return. Otherwise they return void.
- **If-expressions**, which can execute one of two expressions depending on the
  given condition.
- **Casts** to explicitly cast a value to another type.

Expressions can also always be surrounded by (paranthesis).

In formal grammar:
```bnf
<expression> ::
    <variable> | <borrow> |
    <deref> | <literal> |
    <array> | <struct> |
    <indexing> | <accessing> |
    <binary-exp> | <unary-exp> |
    <function-call> | <block> |
    <if-expr> | <cast> |
    ( "(" <expression> ")" )

<variable> :: <ident>
<borrow> :: "&" [ "mut" ] <ident>
<deref> :: "*" <ident>
<array> :: "[" <expression>* "]"
<struct> :: <ident> "{" [ <field> ( "," <field> )* [ "," ] ]
<field> :: <ident> ":" <expression>
<indexing> :: <expression> "[" <integer> "]"
<accessing> :: <expression> "." <ident>
<binary-exp> :: <expression> <binop> <expression>
<unary-exp> :: <unary> <expression>
<function-call> :: <expression> "(" [ <expression> ( "," <expression> )* ] ")"
<if-expr> :: "if" <expression> <expression> [ "else" <expression> ]
<cast> :: <expression> "as" <type>
```

An example of each expression in-order is:
```rust
varname // Variable
&varname // Borrow
*borrowed_varname // Deref
[ varname, 5, 7 ] // Array
Test { first: 4, second: 15 } // Struct
array[0] // Indexing
test.first // Accessing
7 + value // Binop
!bool_value // Unary
func(value, 14) // Function call
if varname {} else {} // If-expression
value as u32 // cast
(value + 2) // Binop within parenthesis
```