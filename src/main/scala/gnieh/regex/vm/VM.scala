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

import scala.annotation.tailrec

import scala.collection.immutable.Queue

/** A virtual machine executing the regular expression code against some input.
 *  A run on some input returns the first match if any. It is stateless, thus executing the same
 *  regular expression against the same input will always return the same result.
 *
 *  This is a thread safe, non-backtracking, tail-recursive implementation allowing
 *  for efficient stack-less executions.
 *
 *  @author Lucas Satabin
 */
object VM {

  /** Executes the given regular expression program with the given string input.
   *  It returns a lazily constructed streamm of all matches in the input.
   */
  def exec(program: Vector[Inst], nbSaved: Int, startIdx: Int, string: String): Option[(Int, Int, Vector[Int])] = {
    //println("executing:")
    //println(util.Debug.print(program))
    //println(s"with input: $string")

    @tailrec
    def loop(idx: Int, threads: Queue[RThread], lastMatch: Option[(Int, Int, Vector[Int])]): Option[(Int, Int, Vector[Int])] = {
      val res =
        if(idx >= string.size)
          step(program, nbSaved, string.length, None, threads)
        else {
          step(program, nbSaved, idx, Some(string(idx)), threads)
        }
      res match {
        case Next(Queue()) =>
          // did not match
          lastMatch
        case Next(threads) =>
          loop(idx + 1, threads, lastMatch)
        case Matched(start, end, saved, threads) if threads.nonEmpty && idx < string.size =>
          // a match was found but if some higher priority threads still exist
          // try if we find a longer match
          loop(idx + 1, threads, Some(start, end, saved))
        case Matched(start, end, saved, threads) =>
          Some(start, end, saved)
      }
    }
    // create and schedule the first thread in which the vritual machine executes the code
    loop(
      startIdx,
      schedule(program, RThread(startIdx, 0, Vector.fill(nbSaved * 2)(-1)), Queue(), startIdx),
      None)
  }

  /* given the list of current thread and the currently inspected character, execute one step */
  private def step(program: Vector[Inst], nbSaved: Int, idx: Int, char: Option[Char], threads: Queue[RThread]) = {
    @tailrec
    def loop(threads: Queue[RThread], acc: Queue[RThread]): StepResult =
      if(threads.isEmpty) {
        // we executed all threads for this step, we can go to the next step
        Next(acc)
      } else {
        val ((RThread(startIdx, pc, saved), tail)) = threads.dequeue
        //println(s"at index: $idx with char: $char")
        //println(s"threads: $threads")
        fetch(program, pc) match {
          case AnyMatch() =>
            // any characters matches
            loop(tail, schedule(program, RThread(if(startIdx >= 0) startIdx else idx, pc + 1, saved), acc, idx + 1))
          case CharMatch(c) if char == Some(c) =>
            // the current character matches the expected one, just try the next thread and save the
            // next instruction in this thread for the next step
            loop(tail, schedule(program, RThread(if(startIdx >= 0) startIdx else idx, pc + 1, saved), acc, idx + 1))
          case CharMatch(_) =>
            // the current character does not match the expected one, discard this thread
            loop(tail, acc)
          case ClassMatch(tree) if char.isDefined && tree.contains(char.get) =>
            // the current character is in the expected class, schedule the next instruction in this thread and try further
            loop(tail, schedule(program, RThread(if(startIdx >= 0) startIdx else idx, pc + 1, saved), acc, idx + 1))
          case ClassMatch(_) =>
            // the current character is not is the expected range, discard this thread
            loop(tail, acc)
          case MatchFound =>
            // a match was found
            Matched(startIdx, idx, saved, acc)
        }
      }
    loop(threads, Queue())
  }

  private def fetch(program: Vector[Inst], pc: Int) =
    if(pc >= 0 && pc < program.size)
      program(pc)
    else
      throw new RuntimeException("Invalid regular expression")

  private def schedule(program: Vector[Inst], thread: RThread, queue: Queue[RThread], idx: Int): Queue[RThread] =
    if(queue.exists(_.pc == thread.pc))
      queue
    else {
      fetch(program, thread.pc) match {
        case Split(i1, i2) =>
          schedule(
            program,
            RThread(thread.startIdx, i2, thread.saved),
            schedule(
              program,
              RThread(thread.startIdx, i1, thread.saved),
              queue,
              idx),
            idx)
        case Jump(i) =>
          schedule(program, RThread(thread.startIdx, i, thread.saved), queue, idx)
        case Save(n) =>
          schedule(program, RThread(thread.startIdx, thread.pc + 1, thread.saved.updated(n, idx)), queue, idx)
        case _ =>
          queue.enqueue(thread)
      }
    }

}

case class RThread(startIdx: Int, pc: Int, saved: Vector[Int])

sealed trait StepResult
final case class Matched(start: Int, end: Int, saved: Vector[Int], threads: Queue[RThread]) extends StepResult
final case class Next(threads: Queue[RThread]) extends StepResult

