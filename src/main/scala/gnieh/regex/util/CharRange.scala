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

case class CharRange(start: Char, end: Char) extends Ordered[CharRange] {

  def contains(c: Char): Boolean =
    c >= start && c <= end

  def intersects(that: CharRange): Boolean =
    this.start <= that.end && this.end >= that.start

  def includes(that: CharRange): Boolean =
    this.start <= that.start && this.end >= that.end

  def union(that: CharRange): CharRange = {
    assume(this.intersects(that), "Trying to take union of disjoint ranges")
    // these are min/max, but it avoids conversion as standard min/max functions return integers
    val start1 = if(this.start <= that.start) this.start else that.start
    val end1 = if(this.end >= that.end) this.end else that.end
    CharRange(start1, end1)
  }

  def diff(that: CharRange): (Option[CharRange], Option[CharRange]) = {
    assume(this.includes(that), "Trying to make substract a range not included in this one")
    if(this == that)
      (None, None)
    else if(this.start == that.start)
      (None, Some(CharRange((that.end + 1).toChar, this.end)))
    else if(this.end == that.end)
      (Some(CharRange(this.start, (that.start - 1).toChar)), None)
    else
      (Some(CharRange(this.start, (that.start - 1).toChar)), Some(CharRange((that.end + 1).toChar, this.end)))
  }

  def compare(that: CharRange): Int =
    this.start -that.start

  def before(c: Char): Boolean =
    this.end < c

  def after(c: Char): Boolean =
    this.start > c

  override def toString =
    if(start == end)
      start.toString
    else
      s"$start-$end"

}

object CharRange {

  def apply(c: Char): CharRange =
    CharRange(c, c)

}

