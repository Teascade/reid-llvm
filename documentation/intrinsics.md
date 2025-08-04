## Intrinsics

Intrinsics are functions that are defined within the language compiler but not
in standard library, thus they do not require importing in order to use (and
trying to re-define these will end up causing issues). Intrinsics include all
pre-existing binary-operators, but also some regular functions and associated
functions (that every type has by-default). This document lists them all (except
for the binary operators, because there are hundreds of those).

### Macro Intrinsics

#### `include_bytes!(path: *char) -> &[u8; _]`

Attempts to load file from `path` (relative to module) into memory and includes
it into the compiled binary directly. Returns a borrow to an array containing
bytes from the file. Array length varies depending on the file contents.

### Associated Intrinsics

#### `<T>::sizeof() -> u64`

Simply returns the size of type `T` in bytes.

```rust
i32::sizeof(); // Returns 4
```

#### `<T>::null() -> *T`

Returns a null-pointer of type `T`.

```rust
i32::null(); // Returns *i32 (null-ptr)
```

#### `<T>::malloc(size: u64) -> *T`

Allocates `T::sizeof() * size` bytes and returns a pointer to `T`.

```rust
i32::malloc(30); // Returns *i32

 // Equivalent to
malloc(i32::sizeof() * 30) as *i32
```

#### `<T>::memcpy(destination: *T, source: *T, size: u64)`

Copies `T::sizeof() * size` bytes from pointer `source` to pointer
`destination`.

```rust
let a = i32::malloc(30);
let b = i32::malloc(30);

// Copies the contents from b to a
i32::memcpy(a, b, 30);
```