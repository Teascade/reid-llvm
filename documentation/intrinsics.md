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


#### `<T>::min(a: T, b: T) -> T`
*Note: (only on integer- and floating-point values)*

Returns the smaller of `a` and `b`.

#### `<T>::max(a: T, b: T) -> T`
*Note: (only on integer- and floating-point values)*

Returns the larger of `a` and `b`.

#### `<T>::abs(value: T) -> T`
*Note: (only on signed integer and floating-point values)*

Returns the absolute value of `value`.

#### `<T>::pow(value: T, exponent: T) -> T`
*Note: (only on floating-point numbers)*

Returns `value` raised to the exponent of `exponent`.

#### `<T>::powi(value: T, exponent: u64) -> T`
*Note: (only on floating-point numbers)*

Returns `value` raised to the exponent of `exponent`.

#### `<T>::sin(value: T) -> T`
*Note: (only on floating-point numbers)*

Calculates sine of `value`

#### `<T>::cos(value: T) -> T`
*Note: (only on floating-point numbers)*

Calculates cosine of `value`

#### `<T>::tan(value: T) -> T`
*Note: (only on floating-point numbers)*

Calculates tangent of `value`

#### `<T>::asin(value: T) -> T`
*Note: (only on floating-point numbers)*

Calculates arcsine of `value`

#### `<T>::acos(value: T) -> T`
*Note: (only on floating-point numbers)*

Calculates arccosine of `value`

#### `<T>::atan(value: T) -> T`
*Note: (only on floating-point numbers)*

Calculates arctangent of `value`

#### `<T>::sinh(value: T) -> T`
*Note: (only on floating-point numbers)*

Calculates hyperbolic sine of `value`

#### `<T>::cosh(value: T) -> T`
*Note: (only on floating-point numbers)*

Calculates hyperbolic cosine of `value`

#### `<T>::tanh(value: T) -> T`
*Note: (only on floating-point numbers)*

Calculates hyperbolic tangent of `value`

#### `<T>::log(value: T) -> T`
*Note: (only on floating-point numbers)*

Returns logₑ of `value`

#### `<T>::log2(value: T) -> T`
*Note: (only on floating-point numbers)*

Returns log₂ of `value`

#### `<T>::log10(value: T) -> T`
*Note: (only on floating-point numbers)*

Returns log₁₀ of `value`

#### `<T>::round(value: T) -> T`
*Note: (only on floating-point numbers)*

Rounds `value` to the nearest integer

#### `<T>::trunc(value: T) -> T`
*Note: (only on floating-point numbers)*

Truncates `value` to the integer nearest to `0`.

#### `<T>::ceil(value: T) -> T`
*Note: (only on floating-point numbers)*

Rounds `value` towards positive infinity.

#### `<T>::floor(value: T) -> T`
*Note: (only on floating-point numbers)*

Rounds `value` towards negative infinity.

#### `<T>::even(value: T) -> T`
*Note: (only on floating-point numbers)*

Rounds `value` to the closest even integer.