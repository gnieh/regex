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

import scala.util.{
  Try,
  Success,
  Failure
}

import scala.annotation.tailrec

class RegexParserException(val offset: Int, msg: String) extends Exception(msg) {

  override def getMessage(): String =
    s"$msg near index $offset"

}

/** Parses a regular expression, accepts POSIX Extended Regular Expression syntax.
 *  The accepted grammar is as follows:
 *  TODO: document the accepted operators
 *
 *  @author Lucas Satabin
 */
class Parser(input: String) {

  require(input != null, "Input regular expression string cannot be null")

  private sealed trait Token
  private case object DOT extends Token
  private case object STAR extends Token
  private case object PLUS extends Token
  private case object OPT extends Token
  private case object PIPE extends Token
  private case object LPAR extends Token
  private case object RPAR extends Token
  private case object LBRACKET extends Token
  private case object RBRACKET extends Token
  private case object LBRACE extends Token
  private case object RBRACE extends Token
  private case object CIRC extends Token
  private case object DOLLAR extends Token
  private case object NUMBER_CLASS extends Token
  private case object WORD_CLASS extends Token
  private case object SPACE_CLASS extends Token
  private case object EOI extends Token
  private final case class CHAR(c: Char) extends Token

  // the lexer state is different when in different parts of the regular exception:
  //  - parsing a flag
  //  - parsing a choice
  //  - parsing some normal part of the regular expression
  private sealed trait LexState
  private case object NormalState extends LexState
  private final case class SetState(previous: LexState) extends LexState

  // offset representing the current position in the input
  private type Offset = Int

  private val NoOffset: Offset = -1

  // stack of already parsed regular expression parts
  private type Stack = List[ReNode]

  def parsed: Try[ReNode] = {
    @tailrec
    def loop(state: LexState, level: Int, stack: Stack, offset: Int): Try[Stack] =
      if(offset >= input.length) {
        Success(stack)
      } else {
        // XXX do not use map here to have a tail recuvrsive function
        parseRe(state, level, stack, offset) match {
          case Success((newState, newLevel, newStack, newOffset)) =>
            //println(newStack)
            loop(newState, newLevel, newStack, newOffset)
          case Failure(e) =>
            Failure(e)
        }
      }

      loop(NormalState, 0, Nil, 0).flatMap { stack =>
        reduceAlternatives(0, stack, input.length).flatMap {
          case List(node) =>
            Success(node)
          case _ =>
            Failure(new RegexParserException(0, "Malformed regular expression"))
        }
      }
  }

  /** Parses one regular expression, returning the new stack of parsed elements
   *  and the new offset if it succeeded */
  private def parseRe(state: LexState, level: Int, stack: Stack, offset: Offset): Try[(LexState, Int, Stack, Offset)] =
    nextToken(state, offset).flatMap {
      case (EOI, newOffset) =>
        Success(state, level, stack, newOffset)
      case (DOT, newOffset) =>
        Success(state, level, AnyChar :: stack, newOffset)
      case (CHAR(c), newOffset) =>
        Success(state, level, SomeChar(c) :: stack, newOffset)
      case (NUMBER_CLASS, newOffset) =>
        // number class is syntactic sugar for [0-9]
        Success(state, level, CharSet(List(CharRange('0', '9'))) :: stack, newOffset)
      case (WORD_CLASS, newOffset) =>
        // word class is syntactic sugar for [A-Za-z0-9_]
        Success(state, level,
          CharSet(
            List(
              CharRange('A', 'Z'),
              CharRange('a', 'z'),
              CharRange('0', '9')
            )
          ) :: stack, newOffset)
      case (SPACE_CLASS, newOffset) =>
        // space class is syntactic sugar for [ \t\r\n\f]
        Success(state, level,
          CharSet(
            List(
              CharRange(' '),
              CharRange('\t'),
              CharRange('\r'),
              CharRange('\n'),
              CharRange('\f')
            )
          ) :: stack, newOffset)
      case (STAR, newOffset) =>
        // zero or more repetition, we pop the last element from the stack and
        // push the new repeated one
        for(newStack <- reduceOne("*", stack, offset, Star))
          yield (state, level, newStack, newOffset)
      case (PLUS, newOffset) =>
        // one or more repetition, we pop the last element from the stack and
        // push the new repeated one
        for(newStack <- reduceOne("+", stack, offset, Plus))
          yield (state, level, newStack, newOffset)
      case (OPT, newOffset) =>
        // optional element, we pop the last element from the stack and
        // push the new repeated one
        for(newStack <- reduceOne("?", stack, offset, Opt))
          yield (state, level, newStack, newOffset)
      case (LPAR, newOffset) =>
        // opening a capturing group, push the temporary marker onto the stack and keep going
        Success(state, level + 1, CapturingStart(level, offset) :: stack, newOffset)
      case (RPAR, newOffset) =>
        // closing capturing group, pop all elements until the matching opening temporary
        // node, and push the new captured one
        for(newStack <- reduceCapturing(level - 1, stack, offset))
          yield (state, level - 1, newStack, newOffset)
      case (PIPE, newOffset) =>
        // alternative, reduce until either an opening capturing group or another alternative
        for(newStack <- reduceAlternatives(level, stack, offset))
          yield (state, level, Alternative(offset) :: newStack, newOffset)
      case (LBRACKET, newOffset) =>
        // character set started
        Success(SetState(state), level + 1, CharSetStart(level, offset) :: stack, newOffset)
      case (RBRACKET, newOffset)  =>
        state match {
          case SetState(prevState) =>
            // character set ended, reduce the character set as an alternative between characters
            // reduce until the matching
            for(newStack <- reduceCharSet(level - 1, stack, offset))
              yield (prevState, level - 1, newStack, newOffset)
          case _ =>
            Success(state, level, SomeChar(']') :: stack, newOffset)
        }
      case (CIRC, _) =>
        Failure(new RegexParserException(offset, "'^' anchor not implemented yet"))
      case (DOLLAR, _) =>
        Failure(new RegexParserException(offset, "'$' anchor not implemented yet"))
      case (LBRACE, _) =>
        Failure(new RegexParserException(offset, "counted repetitions not implemented yet"))
      case (RBRACE, _) =>
        Failure(new RegexParserException(offset, "counted repetitions not implemented yet"))
    }

  /* Pops one element of the stack and pushes the newly contructed one from this one */
  private def reduceOne(meta: String, stack: Stack, offset: Offset, constr: ReNode => ReNode) =
    stack match {
      case (tmp: Temporary) :: tail =>
        Failure(new RegexParserException(tmp.offset, "Malformed regular expression"))
      case node :: tail =>
        Success(constr(node) :: tail)
      case Nil =>
        Failure(new RegexParserException(offset, s"Dangling control meta character '$meta'"))
    }

  /* Pops all elements from the stack until the matching temporary opening node, concatenate
   * the nodes, and pushes the new captured node */
  private def reduceCapturing(level: Int, stack: Stack, offset: Offset) = {
    @tailrec
    def loop(stack: Stack, acc: Option[ReNode]): Try[Stack] =
      stack match {
        case CapturingStart(`level`, _) :: tail =>
          // we found the matching opening node
          Success(Capture(acc.getOrElse(Empty)) :: tail)
        case second :: Alternative(_) :: first :: tail =>
          // reduce captured alternatives when encountering them
          loop(tail, Some(acc.fold(Alt(first, second))(a => Alt(first, Concat(second, a)))))
        case (tmp: Temporary) :: _ =>
          Failure(new RegexParserException(tmp.offset, "Malformed regular expression"))
        case node :: tail =>
          loop(tail, Some(acc.fold(node)(Concat(node, _))))
        case Nil =>
          Failure(new RegexParserException(offset, "Unbalanced closing character ')'"))

      }
    loop(stack, None)
  }

  /* Pops all elements from the stack until the matching temporary opening node,alternate
   * the nodes, and pushes the new alternative node */
  private def reduceCharSet(level: Int, stack: Stack, offset: Offset): Try[Stack] = {
    @tailrec
    def loop(stack: Stack, acc: List[CharRange]): Try[Stack] =
      stack match {
        case CharSetStart(`level`, _) :: tail =>
          // we found the matching opening node
          acc match {
            case Nil =>
              Success(tail)
            case List(CharRange(c1, c2)) if c1 == c2 =>
              Success(SomeChar(c1) :: tail)
            case _ =>
              Success(CharSet(acc) :: tail)
          }
        case (tmp: Temporary) :: _ =>
          Failure(new RegexParserException(tmp.offset, "Malformed regular expression"))
        case _ :: SomeChar('-') :: CharSetStart(`level`, off) :: _ =>
          // malformed range `[-a]'
          Failure(new RegexParserException(off + 1, "Malformed range"))
        case SomeChar(c1) :: SomeChar('-') :: SomeChar(c2) :: tail if c2 <= c1 =>
          // well-formed range
          loop(tail, CharRange(c2, c1) :: acc)
        case SomeChar(c) :: tail =>
          // any other character
          loop(tail, CharRange(c) :: acc)
        case CharSet(chars) :: tail =>
          loop(tail, acc ++ chars)
        case n :: tail =>
          Failure(new RegexParserException(offset, "Malformed character set"))
      }
    loop(stack, Nil)
  }

  /* Pops all the elements fromt the stack until we reach an alternative or an opening group,
   * or the bottom of the stack */
  private def reduceAlternatives(level: Int, stack: Stack, offset: Offset) = {
    @tailrec
    def loop(stack: Stack, acc: Option[ReNode]): Try[Stack] =
      stack match {
        case Alternative(off) :: first :: tail =>
          Success(acc.fold(first)(Alt(first, _)) :: tail)
        case CapturingStart(lvl, _) :: tail if lvl == level - 1 =>
          Success(acc.getOrElse(Empty) :: stack)
        case (tmp: Temporary) :: _ =>
          Failure(new RegexParserException(tmp.offset, "Malformed regular expression"))
        case node :: tail =>
          loop(tail, Some(acc.fold(node)(Concat(node, _))))
        case Nil =>
          Success(List(acc.getOrElse(Empty)))
      }
    loop(stack, None)
  }

  private def escapable(state: LexState, c: Char): Boolean =
    state match {
      case NormalState =>
        ".[{()\\*+?|".contains(c)
      case SetState(_) =>
        "\\[]".contains(c)
    }

  private def classable(c: Char): Boolean =
    "dsw".contains(c)

  private def nextToken(state: LexState, offset: Offset): Try[(Token, Offset)] =
    if(offset >= input.size) {
      // EOI reached
      Success(EOI, offset)
    } else if(input(offset) == '\\') {
      if(offset + 1 < input.size) {
        // still something to read
        if(escapable(state, input(offset + 1))) {
          // escaped character
          Success(CHAR(input(offset + 1)), offset + 2)
        } else if(classable(input(offset + 1))) {
          // character class
          if(input(offset + 1) == 'd') {
            Success(NUMBER_CLASS, offset + 2)
          } else if(input(offset + 1) == 's') {
            Success(SPACE_CLASS, offset + 2)
          } else if(input(offset + 1) == 'w') {
            Success(WORD_CLASS, offset + 2)
          } else {
            Failure(new RegexParserException(offset + 1, s"Unknown escaped character '\\${input(offset + 1)}'"))
          }
        } else {
          Failure(new RegexParserException(offset + 1, s"Unknown escaped character '\\${input(offset + 1)}'"))
        }
      } else {
        Failure(new RegexParserException(offset, "Unterminated escaped character"))
      }
    } else if(escapable(state, input(offset))) {
      if(input(offset) == '.')
        Success(DOT, offset + 1)
      else if(input(offset) == '*')
        Success(STAR, offset + 1)
      else if(input(offset) == '+')
        Success(PLUS, offset + 1)
      else if(input(offset) == '?')
        Success(OPT, offset + 1)
      else if(input(offset) == '|')
        Success(PIPE,  offset + 1)
      else if(input(offset) == '(')
        Success(LPAR,  offset + 1)
      else if(input(offset) == ')')
        Success(RPAR,  offset + 1)
      else if(input(offset) == '[')
        Success(LBRACKET,  offset + 1)
      else if(input(offset) == ']')
        Success(RBRACKET,  offset + 1)
      else if(input(offset) == '^')
        Success(CIRC,  offset + 1)
      else if(input(offset) == '$')
        Success(DOLLAR,  offset + 1)
      else
        Success(CHAR(input(offset)),  offset + 1)
    } else {
      Success(CHAR(input(offset)),  offset + 1)
    }

}

