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

/** An instance of a regular expression that supports DSL methods for
 *  combining them together
 *
 *  @author Lucas Satabin
 */
class DslRegex(val re: ReNode) extends Regex(re, None) {

  /** Matches `this` regular expression followed by `that` regular expression */
  def +(that: DslRegex): DslRegex =
    new DslRegex(Concat(this.re, that.re))

  /** Matches `this` regular expression or `that` regular expression */
  def |(that: DslRegex): DslRegex =
    new DslRegex(Alt(this.re, that.re))

  /** Matches this regular expression zero or more times */
  lazy val zeroOrMore: DslGreedyRegex =
    new DslGreedyStar(re)

  /** Matches this regular expression one or more times */
  lazy val oneOrMore: DslGreedyRegex =
    new DslGreedyPlus(re)

  /** Matches this regular expression zero or one times */
  lazy val optional: DslGreedyRegex =
    new DslGreedyOpt(re)

  /** Matches the same regular expression and captures the result */
  lazy val capture: DslRegex =
    new DslRegex(Capture(re))

}

