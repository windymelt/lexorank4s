/*
 * Copyright 2025 windymelt
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package dev.capslock.lexorank4s

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalacheck.*
import org.scalacheck.Prop.forAll
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class MainSuite extends AnyFunSuite with ScalaCheckPropertyChecks with Matchers {
  val lexoGen = Arbitrary.arbitrary[BigInt].suchThat(_ >= 0).map { n =>
    LexoRank.fromBigInt(0, n)
  }
  implicit val lexoArbitrary: Arbitrary[LexoRank] = Arbitrary(lexoGen)

  test("half radix should be correct") {
    LexoRank.HALF_RADIX_VALUE shouldBe "i"
  }

  test("can make LexoRank") {
    LexoRank("0|i0")
    LexoRank(0, "j")
  }

  test("can make LexoRank from two LexoRank") {
    val r1 = LexoRank("0|i")
    val r2 = LexoRank("0|j")
    val r3 = r1 between r2

    r3 shouldBe LexoRank("0|ii")
  }

  test("all LexoRank can be compared") {
    import LexoRank.given
    import scala.math.Ordered.orderingToOrdered

    forAll { (r1: LexoRank, r2: LexoRank) =>
      r1 <= r2 match {
        case true =>
          if (r1 == r2) {
            r2 shouldBe r1
          } else {
            r2 should be >= r1
          }
        case false =>
          r2 should be < r1
      }
    }
  }

  test("all LexoRank can make between") {
    import LexoRank.given
    import scala.math.Ordered.orderingToOrdered

    forAll { (r1: LexoRank, r2: LexoRank) =>
      whenever(r1.bucket == r2.bucket && r1 != r2) {
        val (l, r) = if (r1 < r2) then (r1, r2) else (r2, r1)
        val x = l between r

        x.bucket shouldBe r.bucket
        x.bucket shouldBe l.bucket

        x should not be l
        x should not be r

        if (l <= r) {
          x should be > l
          x should be < r
        } else {
          x should be < l
          x should be > r
        }
      }
    }
  }

}
