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

import compiler._

import org.scalameter._

class CompilationBenchmark extends PerformanceTest.Microbenchmark {

    val re = Gen.single("re")("([-A-Za-z0-9_.!~*'();/?:@&=+$,# ]|%[A-Fa-f0-9]{2})+")

    performance of "parsing" in {

      measure method "parse" in {

        using(re) in { re =>
          Parser.parse(re)
        }

      }

    }

}

