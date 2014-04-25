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
package util

import scala.annotation.tailrec

/** Represents a set of disjoint ordered character ranges.
 *
 *  @author Lucas Satabin
 */
class CharRangeSet(val ranges: List[CharRange]) extends Serializable {

  def +(range: CharRange): CharRangeSet = {
    @tailrec
    def insert(range: CharRange, ranges: List[CharRange], acc: List[CharRange]): List[CharRange] =
      ranges match {
        case head :: tail if head.intersects(range) =>
          // the range we wish to insert intersects with the current one, merge them
          // and insert the merged range in the rest
          val range1 = head.union(range)
          insert(range1, tail, acc)

        case head :: tail if head.start > range.end =>
          // the range we wish to include is strictly before the current one
          acc reverse_::: (range :: ranges)

        case head :: tail if head.end < range.start =>
          // the range we wish to include is after the current one
          insert(range, tail, head :: acc)

        case Nil =>
          acc reverse_::: List(range)
      }
    new CharRangeSet(insert(range, ranges, Nil))
  }

  def ++(that: CharRangeSet): CharRangeSet =
    that.ranges.foldLeft(this)(_ + _)

  lazy val negate: CharRangeSet = {
    @tailrec
    def loop(enclosing: CharRange, ranges: List[CharRange], acc: List[CharRange]): List[CharRange] =
      ranges match {
        case head :: tail if enclosing.includes(head) =>
          // the current range is included in the enclosing one, allright
          enclosing.diff(head) match {
            case (Some(r1), Some(r2)) =>
              // we were in the middle of the enclosing range, negate the rest
              loop(r2, tail, r1 :: acc)
            case (Some(r), None) =>
              // we reached the end of enclosing range, nothing else to do
              acc reverse_::: List(r)
            case (None, Some(r)) =>
              // we were at the beginning of the enclosing range, negate the rest
              loop(r, tail, acc)
            case (None, None) =>
              // we were equal to the enclosing range, nothing else to do
              acc.reverse
          }
        case head :: tail =>
          // skip this one, not enclosed
          loop(enclosing, tail, acc)
        case Nil =>
          acc reverse_::: List(enclosing)
      }
    new CharRangeSet(loop(CharRange(Char.MinValue, Char.MaxValue), ranges, Nil))
  }

  def toTree: AVL =
    ranges.foldLeft(AVL())(_ + _)

  override def toString =
    ranges.mkString("{", ", ", "}")

}

object CharRangeSet {
  def apply(ranges: CharRange*): CharRangeSet =
    ranges.foldLeft(new CharRangeSet(Nil))(_ + _)

}

