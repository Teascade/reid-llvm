# Standard Library

## Strings

#### `pub struct String`

Editable string value that can be printed and extended

Has the following binops defined:
- `String` + `*char` = `String`
- `String` + `u64` = `String`

##### `String::new() -> String`

Returns a new empty `String`-object, which must later be manually freed.

##### `String::from(str: *char) -> String`

Creates a new `String`-object containing initially data from the given string-literal which must be later freed.

##### `String::set(&mut self, c: char, position: u64)`

Edits given `string` by setting the character at index `position` to be `c`.

##### `String::push_num(&mut self, num: u64)`

Formats the given number into the end of the string.

##### `String::concat(&mut self, source: String)`

Concatenates `source` to the end of `destination`.

### Deprecated functions

#### `pub fn new_string() -> String`

Returns a new empty `String`-object, which must later be manually freed.

_deprecated: Use `String::new()`_

#### `pub fn from_str(string: *char) -> String`

Creates a new `String`-object containing initially data from the given string-literal which must be later freed.

_deprecated: Use `String::from()`_

#### `pub fn set_char(string: &mut String, c: char, position: u64)`

Edits given `string` by setting the character at index `position` to be `c`.

_deprecated: Use `String::set()`_

#### `pub fn add_num_to_string(string: &mut String, num: u64)`

Formats the given number into the end of the string.

_deprecated: Use `String::push_num()`_

#### `pub fn concat_strings(destination: &mut String, source: String)`

Concatenates `source` to the end of `destination`.

_deprecated: Use `String::concat()`_

## General

## Maths

#### `pub fn clamp(min: f32, max: f32, value: f32) -> f32`

Returns `value` as clamped between `min` and `max`. Equivalent to `max(min(value, max), min)`

#### `pub fn abs(value: f32) -> f32`

Returns the absolute value of `value`.