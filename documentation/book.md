# Book of Reid

Welcome to the Book of Reid, a learning resource for Reid programming language.
This is neither just documentation or a tutorial, but something in between,
trying to establish basic concepts and philosophies.

Before reading this book, it is recommended you familiarize yourself with [the
Syntax of Reid](./README.md#syntax-and-general-information). After you're
familiar with that, you can continue here.

The best way to think about Reid is to think about how a combination of Rust's
Syntax and C's closeness to hardware would manifest itself in a language. Reid
is a very grounded language with not many safety features in reality, while
still trying to have some. Reid also has error raporting vastly superior to that
of C, similar to Rust.

Reid also, similarly to Rust, has a powerful typechecker that can infer types
automatically quite well. When this does not pan out, you can often coerce types
either in literals by adding a type after it (e.g. `5u32`), similarly to rust,
or simply casting the value (e.g. `5 as u32`).

## Table of Contents:
- [Hello World](#hello-world)
- [Borrowing and Pointers](#borrowing-and-pointers)

### Hello World

A hello world in Reid looks something like this:

```rust
import std::print;
import std::from_str;
import std::free_string;

fn main() {
    let message = from_str("hello world");
    print(message);
    free_string(&message);
}
```

Let's go through this example line-by-line:

```rust
import std::print;
import std::from_str;
import std::free_string;
```

Tthe first 3 lines are simply imports from the [Standard
Library](./standard_library.md) to functions `print`, `from_str` and
`free_string`, which are used later in this example.

```rust
fn main() {
    ...
}
```

Then we declare our `main`-function. The function that gets executed after
compilation is always called `main`, and it can return a value, although it does
not necessarily have to. The return code of the program ends up being the return
value of `main`, and without a return value it may be unpredictable. In this
example we don't declare a return value for `main`.

```rust
    let message = from_str("hello world");
```

Then we create our printable message with `from_str` and store it in variable
`message`. While this value could be passed to `print` directly, it is necessary
to store the value first in order to free it. Let's come back to that.


```rust
    print(message);
```

Here we actually print out the message we just created, very simple.

```rust
    free_string(&message);
```

Finally we free the string. Like mentioned before, it is necessary to store the
value in a variable so that the memory allocated for the message can be free.
While freeing the memory is not strictly necessary, it is recommended,
especially if the program runs for longer than this example.

That's the Hello World of Reid! It is not a oneliner, but at least I'd say it is quite simple in the end!

### Borrowing and Pointers

In Reid, all **variables** can be borrowed, and borrows can be dereferenced.
Borrows act like pointers, except that borrows do not have the same implicit
safety-problem as pointers, because Borrows are not implicitly unsized. With
pointers, the size of the allocated memory is unknown at compile time, which
makes them unsafe in comparisons.

Note though how **variables** were bolded; You can not make borrows out of just any expressions, they must first be stored in variables. A simple example using borrows would be:
```rust
fn main() -> u32 {
    // Create a value to be mutated
    let mut value = [4, 3, 2];

    // Pass a mutable borrow of the value
    mutate(&mut value);

    // Retrieve the now-mutated value
    return value[1];
}

fn mutate(value: &mut [u32; 3]) {
    // Dereference the borrow to mutate it
    *value[1] = 17;
}
```

This example will always return `17`. Notice also, how a **mutable** borrow was
passed to `mutate`-function. While borrows do not always need to be mutable,
this example would not work without the `mut`-keyword. Try it out for yourself
to see why!