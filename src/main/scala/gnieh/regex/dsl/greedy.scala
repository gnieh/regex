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
package dsl

import util._

/** A greedy operator regular expression. It matches as many characters as possible.
 *
 *  @author Lucas Satabin
 */
trait DslGreedyRegex extends DslRegex {

  /** Turns this greedy operator (matches as many characters as possible) into
   *  a non greedy one (matches as few characters as possible) */
  def nonGreedy: DslRegex

}

private class DslGreedyStar(re: ReNode) extends DslRegex(Star(re, true)) with DslGreedyRegex {

  lazy val nonGreedy: DslRegex =
    new DslRegex(Star(re, false))

}

private class DslGreedyPlus(re: ReNode) extends DslRegex(Plus(re, true)) with DslGreedyRegex {

  lazy val nonGreedy: DslRegex =
    new DslRegex(Plus(re, false))

}

private class DslGreedyOpt(re: ReNode) extends DslRegex(Opt(re, true)) with DslGreedyRegex {

  lazy val nonGreedy: DslRegex =
    new DslRegex(Opt(re, false))

}

