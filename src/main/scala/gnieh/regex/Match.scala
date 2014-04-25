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

/** A match resulting from the application of a regular expression to a string.
 *  This class gives access to matching subgroups, start and end position,
 *  and the matched substring.
 *  Matched group indexes are zero-based. Group index `0` is the entire matching string
 *
 *  @author Lucas Satabin
 */
class Match(val start: Int, val end: Int, groups: Vector[Int], val source: String) extends Serializable {

  /** Returns the substring matching the given group number if any */
  def group(i: Int): Option[String] =
    if(i == 0) {
      matched
    } else {
      val idx = 2 * (i - 1)
      if(idx >= groups.size - 1)
        None
      else {
        val startMatch = groups(idx)
        val endMatch = groups(idx + 1)
        if(startMatch >= 0 && startMatch < endMatch && endMatch <= source.size)
          Some(source.substring(startMatch, endMatch))
        else
          None
      }
    }

  /** Returns the matched substring, or `None` if none */
  def matched: Option[String] =
    if(start >= 0 && start < end && end <= source.size)
      Some(source.substring(start, end))
    else
      None

  /** List of all matched subgroups, not including `group(0)` */
  def subgroups: List[String] =
    for {
      Vector(startMatch, endMatch) <- groups.grouped(2).toList
      if startMatch >= 0 && startMatch < endMatch && endMatch <= source.size
    } yield source.substring(startMatch, endMatch)

}

