package gnieh.sreg

class NFA {
}

sealed trait State
final case class CharState(c: Char) extends State
final case class SplitState(first: State, second: State) extends State

