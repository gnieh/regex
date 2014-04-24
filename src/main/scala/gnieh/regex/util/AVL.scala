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
sealed trait AVL {

  def +(range: CharRange): AVL

  def height: Int

  def factor: Int

  def contains(char: Char): Boolean

  def repaired: AVL

  def leftRotate: AVL

  def rightRotate: AVL

}

object AVL {

  def apply(ranges: CharRange*): AVL =
    ranges.foldLeft(AVLLeaf: AVL)(_ + _)

  def unapplySeq(tree: AVL): Option[List[CharRange]] = tree match {
    case AVLLeaf =>
      Some(Nil)
    case AVLNode(range, left, right) =>
      for {
        l <- unapplySeq(left)
        r <- unapplySeq(right)
      } yield l ++ List(range) ++ r
  }

  def empty: AVL =
    AVLLeaf

}

private final case class AVLNode(range: CharRange, left: AVL, right: AVL) extends AVL {

  def +(r: CharRange): AVL =
    if(r == range)
      this
    else if(r <= range)
      AVLNode(range, left + r, right).repaired
    else
      AVLNode(range, left, right + r).repaired

  def contains(char: Char): Boolean =
    range.contains(char) || (range.after(char) && left.contains(char)) || (range.before(char) && right.contains(char))

  val height: Int =
    1 + math.max(left.height, right.height)

  val factor: Int =
    left.height - right.height

  def repaired: AVL =
    if(factor >= -1 && factor <= 1) {
      this
    } else if(factor == 2) {
      val this1 =
        if(left.factor == -1)
          AVLNode(range, left.leftRotate, right)
        else
          this
      this1.rightRotate
    } else {
      val this1 =
        if(right.factor == 1)
          AVLNode(range, left, right.rightRotate)
        else
          this
      this1.leftRotate
    }

  def leftRotate: AVL =
    right match {
      case AVLNode(range1, left1, right1) =>
        AVLNode(range1, AVLNode(range, left, left1), right1)
      case _ =>
        this
    }

  def rightRotate: AVL =
    left match {
      case AVLNode(range1, left1, right1) =>
        AVLNode(range1, left1, AVLNode(range, right, right1))
      case _ =>
        this
    }

  override def toString =
    s"[$left$range$right]"

}

private case object AVLLeaf extends AVL {

  def +(r: CharRange): AVL =
    AVLNode(r, AVLLeaf, AVLLeaf)

  def contains(char: Char): Boolean =
    false

  val height: Int =
    0

  val factor: Int =
    0

  def leftRotate: AVL =
    this

  def repaired: AVL =
    this

  def rightRotate: AVL =
    this

  override def toString =
    ""

}

