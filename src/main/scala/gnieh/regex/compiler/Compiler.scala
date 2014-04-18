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

import vm._

class Compiler(options: Options) {

  def compile(re: ReNode): Inst = {
    def loop(currentSave: Int, re: ReNode): Inst = {
      re match {
        case Empty =>
          Accept()
        case SomeChar(c) =>
          CharMatch(c, Accept())
        case AnyChar =>
          AnyMatch(Accept())
        case Concat(e1, e2) =>
          loop(currentSave, e1).andThen(loop(currentSave, e2))
        case Alt(e1, e2) =>
          val end = Accept()
          Split(loop(currentSave, e1).andThen(end), loop(currentSave, e2).andThen(end))
        case Opt(e) =>
          val end = Accept()
          Split(loop(currentSave, e).andThen(end), end)
        case Star(e) =>
          lazy val split: Inst = Split(loop(currentSave, e).andThen(Jump(split)), Accept())
          split
        case Plus(e) =>
          lazy val e1: Inst = loop(currentSave, e).andThen(Split(e1, Accept()))
          e1
        case Range(c1, c2) =>
          RangeMatch(c1, c2, Accept())
        case Capture(e) =>
          Save(currentSave, loop(currentSave + 2, e).andThen(Save(currentSave + 1, Accept())))
        case _: Temporary =>
          throw new RuntimeException("Should never happen")
      }
    }
    loop(0, re)
  }

}

