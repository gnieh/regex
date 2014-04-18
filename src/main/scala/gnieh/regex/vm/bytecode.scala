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
  def withNext(i: Inst): Inst
}

final case class CharMatch(c: Char, next: Inst) extends Inst {
  def withNext(i: Inst): Inst =
    CharMatch(c, next.withNext(i))
}

final case class RangeMatch(start: Char, end: Char, next: Inst) extends Inst {
  def withNext(i: Inst): Inst =
    RangeMatch(start, end, next.withNext(i))
}

case object MatchFound extends Inst {
  def withNext(i: Inst): Inst =
    i
}

final case class Split(next1: Inst, next2: Inst) extends Inst {
  def withNext(i: Inst): Inst =
    Split(next1.withNext(i), next2.withNext(i))
}

final case class Jump(next: Inst) extends Inst {
  def withNext(i: Inst): Inst =
    Jump(next.withNext(i))
}

final case class Save(nb: Int, next: Inst) extends Inst {
  def withNext(i: Inst): Inst =
    Save(nb, next.withNext(i))
}

