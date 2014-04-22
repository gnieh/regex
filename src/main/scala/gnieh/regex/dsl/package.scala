/*
* This file is part of the regex project.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package gnieh.regex

import compiler._
import util._

/** This package contains the DSL for building regular expressions.
 *  It is inspired by the [Re](http://re-lib.rubyforge.org/) library.
 *
 *  Following basic constructors are available:
 *   - `any` matches any character,
 *   - `any(classes)` matches any character in the classes. A character class
 *     is either a single character `c`, a range `a-z`, or a built-in class (`\d`, `\w`, `\s`, ...),
 *   - `digit` matches any digit (equivalent to `\d`),
 *   - `digits` matches digits (equivalent to `\d+`),
 *   - `empty` matches the empty string
 *   - `hexDigit` matches any hexadecimal digit (equivalent to `[A-Fa-f0-9]`),
 *   - `hexDigits` matches hexadecimal digits (equivalent to `[A-Fa-f0-9]+`),
 *   - `none(classes)` matches any character that is not in any of the classes,
 *   - `nonspace` matches any non space character (equivalent to `\S`),
 *   - `nonspaces` matches non space characters (equivalent to `\S+`),
 *   - `space` matches any space character (equivalent to `\s`),
 *   - `spaces` matches space characters (equivalent to `\s+`),
 *   - `word` matches any word (equivalent to `\w+`)
 *   - `wordChar` matches any word character (equivalent to `\w`)
 *
 *  @author Lucas Satabin
 */
package object dsl {

  implicit class RichChar(val char: Char) extends AnyVal {

    def --(that: Char): CharSet =
      CharSet(IntervalTree() + CharRange(char, that))

  }

  val digit: DslRegex =
    new DslRegex('0' -- '9')

  val digits: DslRegex =
    new DslRegex(Plus(digit.re))

}

