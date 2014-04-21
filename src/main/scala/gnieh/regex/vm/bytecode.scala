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

import util._

sealed trait Inst

final case class CharMatch(c: Char) extends Inst {
  override def toString = s"char $c"
}

final case class AnyMatch() extends Inst {
  override def toString = "any"
}

final case class ClassMatch(clazz: IntervalTree) extends Inst {
  override def toString = s"class $clazz"
}

case object MatchFound extends Inst {
  override def toString = "match"
}

final case class Split(next1: Int, next2: Int) extends Inst {
  override def toString = s"split $next1, $next2"
}

final case class Jump(next: Int) extends Inst {
  override def toString = s"jump $next"
}

final case class Save(nb: Int) extends Inst {
  override def toString = s"save $nb"
}

