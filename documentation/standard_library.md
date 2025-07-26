# Standard Library

## Strings

#### `pub struct String`

Editable string value that can be printed and extended

Has the following binops defined:
- `String` + `*char` = `String`
- `String` + `u64` = `String`

#### `pub fn print(message: String)`

Prints given `message` to the standard output

#### `pub fn new_string() -> String`

Returns a new empty `String`-object, which must later be manually freed.

#### `pub fn from_str(string: *char) -> String`

Creates a new `String`-object containing initially data from the given string-literal which must be later freed.

#### `pub fn set_char(string: &mut String, c: char, position: u64)`

Edits given `string` by setting the character at index `position` to be `c`.

#### `pub fn add_num_to_string(string: &mut String, num: u64)`

Formats the given number into the end of the string.

#### `pub fn concat_strings(destination: &mut String, source: String)`

Concatenates `source` to the end of `destination`.

## General

#### `pub fn allocate(size: u64) -> *u8`

Unsafely allocates `size` bytes of memory from the stack, and returns a pointer to it, which must be manually freed.

## Maths

#### `pub fn clamp(min: f32, max: f32, value: f32) -> f32`

Returns `value` as clamped between `min` and `max`. Equivalent to `max(min(value, max), min)`

#### `pub fn abs(value: f32) -> f32`

Returns the absolute value of `value`.