# Type cast

Casts scalar data type to each other or fails: 

* `integer`
* `float`
* `binary`
* `list`
* `atom`
* time stamp triple `{integer(), integer(), integer()}` 

The `typecast` interface re-uses a type field notation of [printf format string](https://en.wikipedia.org/wiki/Printf_format_string) due to historical reasons.

* `i` - casts to signed decimal number (integer).
* `f` - casts to double in normal (fixed-point) notation
* `x` - casts to hexadecimal number using lower-case letters
* `s` - casts to "string", the library uses binary for string representation
* `ls` - casts to wide "string"
* `c` - casts to list of characters
* `lc` - casts to wide "string"
* `a` - casts to existing atom
* `atom` - casts to atom, create new if do not exists
* `t` - casts to time stamp triple
