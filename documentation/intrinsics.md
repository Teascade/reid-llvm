## Intrinsics

Intrinsics are functions that are defined within the language compiler but not
in standard library, thus they do not require importing in order to use (and
trying to re-define these will end up causing issues). Intrinsics include all
pre-existing binary-operators, but also some regular functions and associated
functions (that every type has by-default). This document lists them all (except
for the binary operators, because there are hundreds of those).

### Associated Intrinsics

#### `<T>::sizeof() -> u64`

Simply returns the size of type `T` in bits.

```rust
i32::sizeof(); // Returns 32
```

#### `<T>::alloca(size: u64) -> *T`

Allocates `T::sizeof() * size` bits and returns a pointer to `T`.

**Note:** This does not seem to work correctly currently.

```rust
i32::alloca(30); // Returns *i32
```