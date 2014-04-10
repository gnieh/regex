package gnieh.sreg
package vm

sealed trait Inst

final case class CharMatch(c: Char, next: Inst) extends Inst

case object MacthFound extends Inst

final case class Split(next1: Inst, next2: Inst) extends Inst

final case class Jump(next: Inst) extends Inst

final case class Save(nb: Int, next: Inst) extends Inst

