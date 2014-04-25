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
import vm._

import scala.util.Failure

/** This class provides a way to create and use regular expressions. It is a non backtracking implementation
 *  based on the descrition from [Russ Cox](http://swtch.com/~rsc/regexp/).
 *  Following regular expressions are supported:
 *   - `.` any character, possibly including newline (s=true)
 *   - `[xyz]` character class
 *   - `[^xyz]` negated character class
 *   - `\d` a digit character (equivalent to `[0-9]`)
 *   - `\D` a non digit character (equivalent to `[^0-9]`)
 *   - `\w` an alphanumeric character (equivalent to `[A-Za-z0-9_]`)
 *   - `\W` a non alphanumeric character (equivalent to `[^A-Za-z0-9_]`)
 *   - `\s` a space character (equivalent to `[ \t\r\n\f]`)
 *   - `\S` a non space character (equivalent to `[^ \t\r\n\f]`)
 *   - `xy` `x` followed by `y`
 *   - `x|y` `x` or `y` (prefer `x`)
 *   - `x*` zero or more `x` (prefer more)
 *   - `x+` one or more `x` (prefer more)
 *   - `x?` zero or one `x` (prefer one)
 *   - `x*?` zero or more `x` (prefer zero)
 *   - `x+?` one or more `x` (prefer one)
 *   - `x??` zero or one `x` (prefer zero)
 *   - `(re)` numbered capturing group (starting at 1)
 *
 *  @author Lucas Satabin
 */
class Regex(re: ReNode, source: Option[String]) extends Serializable {

  def this(source: String) =
    this(Parser.parse(source).get, Some(source))

  private lazy val (saved, compiled) = Compiler.compile(re)

  //println(util.Debug.print(compiled))

  /** Tells whether this regular expression is matched by the given input */
  def isMatchedBy(input: String): Boolean =
    VM.exec(compiled, saved, input).fold(false) {
      case (start, end, _) =>
        //println(s"$input matches from $start to $end")
        start == 0 && end == input.length
    }

  /** Finds the first match of this regular expression in the input.
   *  If nothing matches, returns `None`*/
  def findFirstIn(input: String): Option[String] =
    for {
      m <- findFirstMatchIn(input)
      matched <- m.matched
    } yield matched

  /** Finds the first match of this regular expression in the input.
   *  If nothing matches, returns `None`*/
  def findFirstMatchIn(input: String): Option[Match] = {
    def find(input: String): Option[Match] =
      VM.exec(compiled, saved, input) match {
        case Some((start, end, groups)) =>
          Some(new Match(start, end, groups, input))
        case None if input.nonEmpty =>
          find(input.tail)
        case None =>
          None
      }
    find(input)
  }

  /** Finds all matches of this regular expression in the input. */
  def findAllIn(input: String): Iterator[String] =
    for {
      m <- findAllMatchIn(input)
      matched <- m.matched
    } yield matched

  /** Finds all matches of this regular expression in the input. */
  def findAllMatchIn(input: String): Iterator[Match] = {
    def loop(input: String): Stream[Match] =
      VM.exec(compiled, saved, input) match {
        case Some((start, end, groups)) =>
          new Match(start, end, groups, input) #:: loop(input.substring(end))
        case None if input.nonEmpty =>
          loop(input.tail)
        case None =>
          Stream.empty
      }
    loop(input).iterator
  }

  def unapplySeq(input: String): Option[List[String]] =
    for {
      (start, end, saved) <- VM.exec(compiled, saved, input)
      if start == 0 && end == input.size && saved.length % 2 == 0
    } yield
      for(Vector(s, e) <- saved.grouped(2).toList)
        yield if(s == -1 || e == -1)
          null
        else
          input.substring(s, e)

  override def toString =
    source.getOrElse(re.toString)

}

object Regex {

  def apply(str: String): Regex =
    new Regex(str)

}

