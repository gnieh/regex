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
package compiler

import util._

sealed trait ReNode

case object Empty extends ReNode {
  override def toString = ""
}

case object AnyChar extends ReNode {
  override def toString = "."
}

final case class SomeChar(c: Char) extends ReNode {
  override def toString = c.toString
}

final case class Concat(n1: ReNode, n2: ReNode) extends ReNode {
  override def toString = s"$n1$n2"
}

final case class Alt(n1: ReNode, n2: ReNode) extends ReNode {
  override def toString = s"$n1|$n2"
}

final case class Star(n: ReNode) extends ReNode {
  override def toString = s"$n*"
}

final case class Plus(n: ReNode) extends ReNode {
  override def toString = s"$n+"
}

final case class Opt(n: ReNode) extends ReNode {
  override def toString = s"$n?"
}

final case class CharSet(chars: IntervalTree) extends ReNode {
  override def toString = s"$chars"
}

final case class Capture(n: ReNode) extends ReNode {
  override def toString = s"($n)"
}

/* A temporary node that is pushed onto the parsing stack and serves as marker
 * Typically, this is an opening parenthesis or bracket. */
private[compiler] sealed trait Temporary extends ReNode {
  val offset: Int
}
private[compiler] final case class CapturingStart(level: Int, offset: Int) extends Temporary
private[compiler] final case class CharSetStart(level: Int, offset: Int) extends Temporary
private[compiler] final case class Alternative(offset: Int) extends Temporary

