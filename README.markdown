Gnieh Regex
===========

An efficient non backtracking regular expression implementation in scala.

This is based on the pikevm as described by [Russ Cox](http://swtch.com/~rsc/regexp/)

Why This Library?
-----------------

Yes, I am aware Java has [support for regular expression](http://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html) since version 1.4.
And yes it is [integrated in the Scala standard library](http://www.scala-lang.org/files/archive/api/current/#scala.util.matching.Regex).

But This project is born due to some stack overflows we encountered with alternatives on big inputs.
This is a consequence of how regular expression engine is implemented. Actually, Java regular expressions are too expressive and allow for back references.
For our purpose, we never needed it, and if this feature is not present, there exist quite efficient ways to implement regular expressions with no backtracking and no stack use (with tail-call recursions).

This is how this project was born.

Moreover, it is really fun to implement a regular expression engine anyway, so even if nobody else uses this library, it was really enjoyable to do.

Features
--------

Following regular expressions are supported:
 - `.` any character
 - `[xyz]` character class
 - `[^xyz]` negated character class
 - `\d` a digit character (equivalent to `[0-9]`)
 - `\D` a non digit character (equivalent to `[^0-9]`)
 - `\w` an alphanumeric character (equivalent to `[A-Za-z0-9_]`)
 - `\W` a non alphanumeric character (equivalent to `[^A-Za-z0-9_]`)
 - `\s` a space character (equivalent to `[ \t\r\n\f]`)
 - `\S` a non space character (equivalent to `[^ \t\r\n\f]`)
 - `xy` `x` followed by `y`
 - `x|y` `x` or `y` (prefer `x`)
 - `x*` zero or more `x` (prefer more)
 - `x+` one or more `x` (prefer more)
 - `x?` zero or one `x` (prefer one)
 - `x*?` zero or more `x` (prefer zero)
 - `x+?` one or more `x` (prefer one)
 - `x??` zero or one `x` (prefer zero)
 - `(re)` numbered capturing group (starting at 1)

There is also a DSL inspired by [Re](http://re-lib.rubyforge.org/), to be found in package `gnieh.regex.dsl`.


Getting Started
---------------

To build a regular expression from a string, you just need to write this:
```scala
import gnieh.regex._

val date = """(\d\d\d\d)-(\d\d)-(\d\d)""".re

val date(year, month, day) = "2014-04-23"

```

See the documentation of this library for more details.

