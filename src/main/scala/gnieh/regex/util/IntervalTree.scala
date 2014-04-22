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

/** A tree storing character ranges that is used to efficiently
 *  determine whether a character is in a given class.
 *  It is implemented as an augmented AVL tree.
 *
 *  @author Lucas Satabin
 */
sealed trait IntervalTree {

  def +(range: CharRange): IntervalTree

  def ++(that: IntervalTree): IntervalTree

  def -(range: CharRange): IntervalTree

  def --(that: IntervalTree): IntervalTree

  def max: Char

  def height: Int

  def factor: Int

  def contains(char: Char): Boolean

  def repair: IntervalTree

  def leftRotate: IntervalTree

  def rightRotate: IntervalTree

}

object IntervalTree {

  val FullRange: IntervalTree = IntervalTree(CharRange(Char.MinValue, Char.MaxValue))

  def apply(ranges: CharRange*): IntervalTree =
    ranges.foldLeft(Leaf: IntervalTree)(_ + _)

  def unapplySeq(tree: IntervalTree): Option[List[CharRange]] = tree match {
    case Leaf =>
      Some(Nil)
    case Node(range, _, left, right) =>
      for {
        l <- unapplySeq(left)
        r <- unapplySeq(right)
      } yield l ++ List(range) ++ r
  }

  def empty: IntervalTree =
    Leaf

}

private final case class Node(range: CharRange, max: Char, left: IntervalTree, right: IntervalTree) extends IntervalTree {

  def +(r: CharRange): IntervalTree =
    if(r == range)
      this
    else if(r.start <= range.start)
      Node(range, math.max(max, r.end).toChar, left + r, right).repair
    else
      Node(range, math.max(max, r.end).toChar, left, right + r).repair

  def -(r: CharRange): IntervalTree = {
    val this1 =
      if(range == r)
        Leaf
      else if(range.intersects(r))
        if(range.start < r.start && range.end > r.end)
          Leaf + CharRange(range.start, (r.start - 1).toChar) + CharRange((r.end + 1).toChar, range.end)
        else if(range.start < r.start)
          Leaf + CharRange(range.start, (r.start - 1).toChar)
        else
          Leaf + CharRange((r.end + 1).toChar, range.end)
      else
        this
    if(r.end > max)
      this1
    else
      this1 ++ (left - r) ++ (right - r)
  }

  def --(that: IntervalTree): IntervalTree = that match {
    case Node(range1, _, left1, right1) =>
      this - range1 -- left1 -- right1
    case Leaf =>
      this
  }

  def ++(that: IntervalTree): IntervalTree = that match {
    case Node(range1, _, left1, right1) =>
      this + range1 ++ left1 ++ right1
    case Leaf =>
      this
  }

  def contains(char: Char): Boolean =
    range.contains(char) || (char <= max && (left.contains(char) || right.contains(char)))

  val height: Int =
    1 + math.max(left.height, right.height)

  val factor: Int =
    left.height - right.height

  def repair: IntervalTree =
    if(factor >= -1 && factor <= 1) {
      this
    } else if(factor == 2) {
      val this1 =
        if(left.factor == -1)
          Node(range, max, left.leftRotate, right)
        else
          this
      this1.rightRotate
    } else {
      val this1 =
        if(right.factor == 1)
          Node(range, max, left, right.rightRotate)
        else
          right
      this1.leftRotate
    }


  def leftRotate: IntervalTree =
    right match {
      case Node(range1, max1, left1, right1) =>
        Node(range1, max, Node(range, math.max(left.max, left1.max).toChar, left, left1), right1)
      case _ =>
        this
    }

  def rightRotate: IntervalTree =
    left match {
      case Node(range1, max1, left1, right1) =>
        Node(range1, max, left1, Node(range, math.max(right.max, right1.max).toChar, right, right1))
      case _ =>
        this
    }

  override def toString =
    s"[$left$range$right]"

}

private case object Leaf extends IntervalTree {

  def +(r: CharRange): IntervalTree =
    Node(r, r.end, Leaf, Leaf)

  def -(range: CharRange): IntervalTree =
    this

  def --(that: IntervalTree): IntervalTree =
    this

  def ++(that: IntervalTree): IntervalTree =
    that

  val max = Char.MinValue

  def contains(char: Char): Boolean =
    false

  val height: Int =
    0

  val factor: Int =
    0

  def leftRotate: IntervalTree =
    this

  def repair: IntervalTree =
    this

  def rightRotate: IntervalTree =
    this

  override def toString =
    ""

}
