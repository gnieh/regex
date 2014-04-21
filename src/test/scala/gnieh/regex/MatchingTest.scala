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

import org.scalatest._

/** Tests that various regular expressions features are correctly implemented
 *  by testing matches against strings.
 *
 *  @author Lucas Satabin
 */
class MatchingTest extends FlatSpec with ShouldMatchers {

  "A single character regular expression" should "match iff the string is this character" in {

    val re = "a".re

    re.isMatchedBy("a") should be(true)

  }

  it should "not match a string containing many times this character" in {

    val re = "a".re

    re.isMatchedBy("aaa") should be(false)

  }

  it should "not match a string containing another character" in {

    val re = "a".re

    re.isMatchedBy("ab") should be(false)

  }

  it should "not match a string a single other character" in {

    val re = "a".re

    re.isMatchedBy("b") should be(false)

  }

  "Alternative" should "match if at least one possibility matches the string" in {

    val re = "ab|ac|ad|a.".re

    re.isMatchedBy("ac") should be(true)
    re.isMatchedBy("ab") should be(true)
    re.isMatchedBy("ad") should be(true)
    re.isMatchedBy("ae") should be(true)

  }

  it should "not match if the string does not matche any possibility" in {

    val re = "ab|ac|ad|a.".re

    re.isMatchedBy("abad") should be(false)

  }

  "Character set" should "match if the string is contained in this set" in {

    val re = "[a-zA-Z_][a-z[A-Z]\\d_]*".re

    re.isMatchedBy("some_identifier43") should be(true)

  }

  it should "not match if at a character is not in the set" in {

    val re = "[a-zA-Z_][a-zA-Z0-9_]*".re

    re.isMatchedBy("98toto") should be(false)
    re.isMatchedBy("tété") should be(false)

  }

  "Optional character" should "match if present" in {

    val re = "a?".re

    re.isMatchedBy("a") should be(true)

  }

  it should "match if not present" in {

    val re = "a?".re

    re.isMatchedBy("") should be(true)

  }

  it should "not match if some other character is present" in {

    val re = "a?".re

    re.isMatchedBy("b") should be(false)

  }

  "Starred character" should "match if present once" in {

    val re = "a*".re

    re.isMatchedBy("a") should be(true)

  }

  it should "match if present several times" in {

    val re = "a*".re

    re.isMatchedBy("aaaaaaaaaaaaaa") should be(true)

  }

  it should "match if not present" in {

    val re = "a*".re

    re.isMatchedBy("") should be(true)

  }

  it should "not match if at least one other character is present" in {

    val re = "a*".re

    re.isMatchedBy("aaaaabaaaaaa") should be(false)

  }

  "Plus character" should "match if present once" in {

    val re = "a+".re

    re.isMatchedBy("a") should be(true)

  }

  it should "match if present several times" in {

    val re = "a+".re

    re.isMatchedBy("aaaaaaaaaaaaaa") should be(true)

  }

  it should "not match if not present" in {

    val re = "a+".re

    re.isMatchedBy("") should be(false)

  }

  it should "not match if at least one other character is present" in {

    val re = "a+".re

    re.isMatchedBy("aaaaabaaaaaa") should be(false)

  }

}

