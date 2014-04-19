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

class VM(program: Vector[Inst], nbSaved: Int) {

  /** Executes the given regular expression program with the given string input.
   *  It returns a lazily constructed streamm of all matches in the input.
   */
  def exec(string: String): Option[(Int, Int, Vector[Int])] = {
    //println("executing:")
    //println(util.Debug.print(program))
    //println(s"with input: $string")

    @tailrec
    def loop(string: Seq[(Char, Int)], threads: List[RThread], lastMatch: Option[(Int, Int, Vector[Int])]): Option[(Int, Int, Vector[Int])] =
      if(string.isEmpty)
        lastMatch
      else {
        val (char, idx) = string.head
        step(idx, char, threads) match {
          case Next(Nil) =>
            // did not match
            lastMatch
          case Next(threads) =>
            loop(string.tail, threads, lastMatch)
          case Matched(start, end, saved, threads) if threads.nonEmpty =>
            // a match was found but if some higher priority threads still exist
            // try if we find a longer match
            loop(string.tail, threads, Some(start, end, saved))
          case Matched(start, end, saved, threads) =>
            Some(start, end, saved)
        }
      }
    // create the first thread in which the vritual machine executes the code
    loop(string.zipWithIndex, List(RThread(-1, 0, Vector.fill(nbSaved * 2)(-1))), None)
  }

  /* given the list of current thread and the currently inspected character, execute one step */
  private def step(idx: Int, char: Char, threads: List[RThread]) = {
    @tailrec
    def loop(threads: List[RThread], acc: List[RThread]): StepResult = threads match {
      case RThread(startIdx, pc, saved) :: tail =>
        //println(s"at index: $idx with char: $char")
        //println(s"threads: $threads")
        fetch(pc) match {
          case AnyMatch() =>
            // any characters matches
            loop(tail, schedule(RThread(if(startIdx >= 0) startIdx else idx, pc + 1, saved), acc))
          case CharMatch(c) if char == c =>
            // the current character matches the expected one, just try the next thread and save the
            // next instruction in this thread for the next step
            loop(tail, schedule(RThread(if(startIdx >= 0) startIdx else idx, pc + 1, saved), acc))
          case CharMatch(_) =>
            // the current character does not match the expected one, discard this thread
            loop(tail, acc)
          case RangeMatch(start, end) if char >= start && char <= end =>
            // the current character is in the expected range, schedule the next instruction in this thread and try further
            loop(tail, schedule(RThread(if(startIdx >= 0) startIdx else idx, pc + 1, saved), acc))
          case RangeMatch(_, _) =>
            // the current character is not is the expected range, discard this thread
            loop(tail, acc)
          case MatchFound =>
            // a match was found
            Matched(startIdx, idx, saved, acc)
          case Jump(next) =>
            // simply jump to the next instruction
            loop(RThread(startIdx, next, saved) :: tail, acc)
          case Split(next1, next2) =>
            // spawn a new thread and execute first instruction set in the current thread
            loop(RThread(startIdx, next1, saved) :: RThread(startIdx, next2, saved) :: tail, acc)
          case Save(i) =>
            // save the current index in the appropriate register and try further
            loop(RThread(startIdx, pc + 1, saved.updated(i, idx)) :: tail, acc)
        }
      case Nil =>
        // we executed all threads for this step, we can go to the next step
        Next(acc)
    }
    loop(threads, Nil)
  }

  private def fetch(pc: Int) =
    if(pc >= 0 && pc < program.size)
      program(pc)
    else
      throw new RuntimeException("Invalid regular expression")

  private def schedule(thread: RThread, queue: List[RThread]): List[RThread] =
    if(queue.exists(_.pc == thread.pc))
      queue
    else
      thread :: queue

}

case class RThread(startIdx: Int, pc: Int, saved: Vector[Int])

sealed trait StepResult
final case class Matched(start: Int, end: Int, saved: Vector[Int], threads: List[RThread]) extends StepResult
final case class Next(threads: List[RThread]) extends StepResult

