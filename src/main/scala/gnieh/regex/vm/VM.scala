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

import scala.annotation.tailrec

object VM {

  /** Executes the given regular expression program with the given string input */
  def exec(string: String, program: Inst, nbSaved: Int): Option[Vector[Int]] = {
    @tailrec
    def loop(string: Seq[(Char, Int)], threads: List[RThread]): Option[Vector[Int]] =
      if(string.isEmpty)
        None
      else {
        val (char, idx) = string.head
        step(idx, char, threads) match {
          case Next(Nil) =>
            // did not match
           None
          case Next(threads) =>
            loop(string.tail, threads)
          case Matched(saved) =>
            Some(saved)
        }
      }
    // create the first thread in which the vritual machine executes the code
    loop(string.zipWithIndex, List(RThread(program, Vector.fill(nbSaved)(-1))))
  }

  /* given the list of current thread and the currently inspected character, execute one step */
  private def step(idx: Int, char: Char, threads: List[RThread]) = {
    @tailrec
    def loop(threads: List[RThread], acc: Set[RThread]): StepResult = threads match {
      case RThread(CharMatch(c, next), saved) :: tail if char == c =>
        // the current character matches the expected one, just try the next thread and save the
        // next instruction in this thread for the next step
        loop(tail, acc + RThread(next, saved))
      case RThread(CharMatch(_, _), _) :: tail =>
        // the current character does not match the expected one, discard this thread
        loop(tail, acc)
      case RThread(RangeMatch(start, end, next), saved) :: tail if char >= start && char <= end =>
        loop(tail, acc + RThread(next, saved))
      case RThread(RangeMatch(_, _, _), _) :: tail =>
        loop(tail, acc)
      case RThread(MatchFound, saved) :: tail =>
        Matched(saved)
      case RThread(Jump(next), saved) :: tail =>
        loop(RThread(next, saved) :: tail, acc)
      case RThread(Split(next1, next2), saved) :: tail =>
        loop(RThread(next1, saved) :: RThread(next2, saved) :: tail, acc)
      case RThread(Save(i, next), saved) :: tail =>
        loop(RThread(next, saved.updated(i, idx)) :: tail, acc)
      case Nil =>
        Next(acc.toList)
    }
    loop(threads, Set())
  }

}

case class RThread(pc: Inst, saved: Vector[Int])

sealed trait StepResult
final case class Matched(saved: Vector[Int]) extends StepResult
final case class Next(threads: List[RThread]) extends StepResult

