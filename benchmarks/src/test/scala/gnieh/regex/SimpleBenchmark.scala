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

import org.scalameter._

class SimpleBenchmark extends PerformanceTest.Quickbenchmark {

  val text = """Et licet quocumque oculos flexeris feminas adfatim multas spectare cirratas, quibus, si nupsissent, per aetatem ter iam
               |nixus poterat suppetere liberorum, ad usque taedium pedibus pavimenta tergentes iactari volucriter gyris, dum exprimunt innumera
               |simulacra, quae finxere fabulae theatrales.%0A%0AHaec subinde Constantius audiens et quaedam referente Thalassio doctus, quem eum odisse
               |iam conpererat lege communi, scribens ad Caesarem blandius adiumenta paulatim illi subtraxit, sollicitari se simulans ne, uti est militare
               |otium fere tumultuosum, in eius perniciem conspiraret, solisque scholis iussit esse contentum palatinis et protectorum cum Scutariis et
               |Gentilibus, et mandabat Domitiano, ex comite largitionum, praefecto ut cum in Syriam venerit, Gallum, quem crebro acciverat, ad Italiam
               |properare blande hortaretur et verecunde.%0A%0ASed si ille hac tam eximia fortuna propter utilitatem rei publicae frui non properat, ut
               |omnia illa conficiat, quid ego, senator, facere debeo, quem, etiamsi ille aliud vellet, rei publicae consulere oporteret?%0AEt licet
               |quocumque oculos flexeris feminas adfatim multas spectare cirratas, quibus, si nupsissent, per aetatem ter iam nixus poterat suppetere
               |liberorum, ad usque taedium pedibus pavimenta tergentes iactari volucriter gyris, dum exprimunt innumera simulacra, quae finxere fabulae
               |theatrales.%0A%0AHaec subinde Constantius audiens et quaedam referente Thalassio doctus, quem eum odisse iam conpererat lege communi,
               |scribens ad Caesarem blandius adiumenta paulatim illi subtraxit, sollicitari se simulans ne, uti est militare otium fere tumultuosum, in
               |eius perniciem conspiraret, solisque scholis iussit esse contentum palatinis et protectorum cum Scutariis et Gentilibus, et mandabat
               |Domitiano, ex comite largitionum, praefecto ut cum in Syriam venerit, Gallum, quem crebro acciverat, ad Italiam properare blande
               |hortaretur et verecunde.%0A%0ASed si ille hac tam eximia fortuna propter utilitatem rei publicae frui non properat, ut omnia illa
               |conficiat, quid ego, senator, facere debeo, quem, etiamsi ille aliud vellet, rei publicae consulere oporteret?%0AEt licet quocumque oculos
               |flexeris feminas adfatim multas spectare cirratas, quibus, si nupsissent, per aetatem ter iam nixus poterat suppetere liberorum, ad usque
               |taedium pedibus pavimenta tergentes iactari volucriter gyris, dum exprimunt innumera simulacra, quae finxere fabulae theatrales.%0A%0AHaec
               |subinde Constantius audiens et quaedam referente Thalassio doctus, quem eum odisse iam conpererat lege communi, scribens ad Caesarem
               |blandius adiumenta paulatim illi subtraxit, sollicitari se simulans ne, uti est militare otium fere tumultuosum, in eius perniciem
               |conspiraret, solisque scholis iussit esse contentum palatinis et protectorum cum Scutariis et Gentilibus, et mandabat Domitiano, ex comite
               |largitionum, praefecto ut cum in Syriam venerit, Gallum, quem crebro acciverat, ad Italiam properare blande hortaretur et
               |verecunde.%0A%0ASed si ille hac tam eximia fortuna propter utilitatem rei publicae frui non properat, ut omnia illa conficiat, quid ego,
               |senator, facere debeo, quem, etiamsi ille aliud vellet, rei publicae consulere oporteret?%0A""".stripMargin.replace("\n", "")

  val textGen = Gen.single("text")(text)

  val reGen =
    for(re <- Gen.single("re")("([-A-Za-z0-9_.!~*'();/?:@&=+$,# ]|%[A-Fa-f0-9]{2})+".re))
      yield {
        // force evaluation to make it compile
        re.isMatchedBy("")
        re
      }

  val inputs = Gen.tupled(textGen, reGen)

  performance of "New regular expression" in {
    measure method "findFirstIn" in {

      using(textGen) in { t =>
        val localRe = "([-A-Za-z0-9_.!~*'();/?:@&=+$,# ]|%[A-Fa-f0-9]{2})+".re

        localRe.findFirstIn(t)
      }

    }
  }

  performance of "Reused regular expression" in {
    measure method "findFirstIn" in {

      using(inputs) in { case (t, re) =>
        re.findFirstIn(t)
      }

    }
  }

}

