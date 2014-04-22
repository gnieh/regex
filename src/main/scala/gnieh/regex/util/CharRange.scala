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

case class CharRange(start: Char, end: Char) {

  def contains(c: Char): Boolean =
    c >= start && c <= end

  def intersects(that: CharRange): Boolean =
    this.start <= that.end && this.end >= that.start

  def includes(that: CharRange): Boolean =
    this.start <= that.start && this.end >= that.end

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

