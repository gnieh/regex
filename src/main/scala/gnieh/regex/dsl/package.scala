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

import scala.language.implicitConversions

/** This package contains the DSL for building regular expressions.
 *  It is inspired by the [Re](http://re-lib.rubyforge.org/) library.
 *  It also provides some useful implicit conversions to be used more conveniently.
 *
 *  @author Lucas Satabin
 */
package object dsl {

  implicit class RichChar(val char: Char) extends AnyVal {

    def --(that: Char): CharRange =
      CharRange(char, that)

  }

  implicit def char2charrange(c: Char): CharRange =
    CharRange(c)

  /** Matches any character */
  lazy val any: DslRegex =
    new DslRegex(AnyChar)

  /** Matches any character in the classes. A character class
   *  is either a single character `c`, a range `a-z`
   */
  def any(classes: CharRange*): DslRegex =
    new DslRegex(CharSet(IntervalTree(classes: _*)))

  /** Matches any digit (equivalent to `[0-9]`) */
  lazy val digit: DslRegex =
    any('0' -- '9')

  /** Matches digits (equivalent to `[0-9]+`) */
  lazy val digits: DslRegex =
    digit.oneOrMore

  /** Matches the empty string */
  lazy val empty: DslRegex =
    new DslRegex(Empty)

  /** Matches any hexadecimal digit (equivalent to `[A-Fa-f0-9]`) */
  lazy val hexDigit: DslRegex =
    any('A' -- 'F', 'a' -- 'f', '0' -- '9')

  /** Matches hexadecimal digits (equivalent to `[A-Fa-f0-9]+`) */
  lazy val hexDigits: DslRegex =
    hexDigits.oneOrMore

  /** Matches any character that is not in any of the classes */
  def none(classes: CharRange*): DslRegex =
    new DslRegex(CharSet(IntervalTree.FullRange -- classes))

  /** Matches any non space character (equivalent to `\S`) */
  lazy val nonspace: DslRegex =
    none(' ', '\t', '\r', '\n', '\f')

  /** Matches non space characters (equivalent to `\S+`) */
  lazy val nonspaces: DslRegex =
     nonspace.oneOrMore

  /** Matches the literal characters of the string (special regular expression characters
   *  are considered as raw characters */
  def raw(str: String): DslRegex =
    new DslRegex(str.map(SomeChar(_)).foldLeft(Empty: ReNode)(Concat(_, _)))

  /** Matches any space character (equivalent to `\s`) */
  lazy val space: DslRegex =
    any(' ', '\t', '\r', '\n', '\f')

  /** Matches space characters (equivalent to `\s+`) */
  lazy val spaces: DslRegex =
    space.oneOrMore

  /** Matches any word (equivalent to `\w+`) */
  lazy val word: DslRegex =
    wordChar.oneOrMore

  /** Matches any word character (equivalent to `\w`) */
  lazy val wordChar: DslRegex =
    any('A' -- 'Z', 'a' -- 'z', '0' -- '9', '_')

}

