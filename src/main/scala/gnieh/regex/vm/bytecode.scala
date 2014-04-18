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
package vm

sealed trait Inst {
  def andThen(i: Inst): Inst
}

final case class CharMatch(c: Char, next: Inst) extends Inst {
  def andThen(i: Inst): Inst =
    CharMatch(c, next.andThen(i))
}

final case class AnyMatch(next: Inst) extends Inst {
  def andThen(i: Inst): Inst =
    AnyMatch(next.andThen(i))
}

final case class RangeMatch(start: Char, end: Char, next: Inst) extends Inst {
  def andThen(i: Inst): Inst =
    RangeMatch(start, end, next.andThen(i))
}

case object MatchFound extends Inst {
  def andThen(i: Inst): Inst =
    this
}

final class Split(_next1: =>Inst, _next2: =>Inst) extends Inst {

  lazy val next1 = _next1
  lazy val next2 = _next2

  def andThen(i: Inst): Inst =
    Split(next1.andThen(i), next2.andThen(i))

}
object Split {

  def apply(next1: =>Inst, next2: =>Inst): Split =
    new Split(next1, next2)

  def unapply(inst: Inst): Option[(Inst, Inst)] =
    inst match {
      case split: Split =>
        Some(split.next1 -> split.next2)
      case _ =>
        None
    }

}

final case class Jump(next: Inst) extends Inst {
  def andThen(i: Inst): Inst =
    Jump(next.andThen(i))
}

final case class Save(nb: Int, next: Inst) extends Inst {
  def andThen(i: Inst): Inst =
    Save(nb, next.andThen(i))
}

private[regex] final case class Accept() extends Inst {
  def andThen(i: Inst): Inst =
    i
}

