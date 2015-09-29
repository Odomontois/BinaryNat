/**
 * Author: Oleg Nizhnik
 * Date  : 29.09.2015
 * Time  : 13:24
 */
package com.awt
import shapeless.Poly2

package object binnat extends BNatTypes with BLessImpl with BSumImpl {
  trait LowLevelLess extends Poly2 {
    implicit def defaultIsFalse[A <: BNat, B <: BNat]: Case.Aux[A, B, Boolean] = at[A, B]((_, _) ⇒ false)
  }

  object LessOp extends LowLevelLess {
    implicit def lessIsTrue[A <: BNat, B <: BNat](implicit lt: A #<# B): Case.Aux[A, B, Boolean] = at[A, B]((_, _) ⇒ true)
  }

  implicit class BNatOps[N <: BNat](val n: N) extends AnyVal {
    def +[M <: BNat, Out <: BNat](m: M)(implicit sum: #+#.Aux[N, M, Out], v: Out): Out = v
  }

  implicit class BNonZeroOps[N <: BNonZero](val n: N) extends AnyVal {
    def ##[D <: BDigit](d: D): N ## D = new BSucc[D, N](d, n)

    def _1 = ##(BOne)

    def _0 = ##(BZero)

    def _1[D <: BDigit](d: D) = ##(BOne) ## d

    def _0[D <: BDigit](d: D) = ##(BZero) ## d
  }

  def unmask[N <: BNat](x: N)(implicit n: N): N = n
}
