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
package compiler

sealed trait ReNode

case object Empty extends ReNode

case object AnyChar extends ReNode

final case class SomeChar(c: Char) extends ReNode

final case class Concat(n1: ReNode, n2: ReNode) extends ReNode

final case class Alt(n1: ReNode, n2: ReNode) extends ReNode

final case class Star(n: ReNode) extends ReNode

final case class Plus(n: ReNode) extends ReNode

final case class Range(start: Char, end: Char) extends ReNode

final case class Capture(n: ReNode) extends ReNode

