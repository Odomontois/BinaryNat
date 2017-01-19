package bnat

import BNat._

import scala.language.implicitConversions

class BQuotRem[N <: BNat, D <: BNonZero] private[bnat]() {
  type Quot <: BNat
  type Rem <: BNat
}

trait BQuotImpl {
  implicit def zeroQuot[D <: BNonZero] = BQuotRem.aux[_0_, D, _0_, _0_]
}

trait BQuotImplStep1 extends BQuotImplStep2 {
  implicit def lesserQuot[N <: BNat, D <: BNonZero](implicit less: N #<# D) = BQuotRem.aux[N, D, _0_, N]
}

trait BQuotImplStep2 {
  implicit def biLesserQuot[N <: BNat, D <: BNonZero](implicit minus: BEven[D] #-# N) = BQuotRem.aux[N, D, _1_, minus.Out]
}

trait BQuotImplStep3 {
  implicit def biRestQuot[N <: BNat, D <: BNonZero, Q2 <: BNat, R2 <: BNat, QA <: BNat, R <: BNat]
  (implicit quot: BQuotRem.Aux[N, BEven[D], Q2, R2],
   remQ: BQuotRem.Aux[R2, D, QA, R],
   sumQ: Q2 #+# QA) =
    BQuotRem.aux[N, D, sumQ.Out, R]
}

object BQuotRem extends BQuotImpl {
  type Aux[N <: BNat, D <: BNonZero, Q <: BNat, R <: BNat] = BQuotRem[N, D] {type Quot = Q; type Rem = R}
  private[bnat] def aux[N <: BNat, D <: BNonZero, Q <: BNat, R <: BNat]: Aux[N, D, Q, R] =
    new BQuotRem[N, D] {
      type Quot = Q
      type Rem = R
    }
}

class BQuotOps[A <: BNat](val num: A) extends AnyVal {
  def /[B <: BNonZero, Q <: BNat, R <: BNat](b: B)(implicit quotRem: BQuotRem.Aux[A, B, Q, R], snat: Q): quotRem.Quot = snat
  def \[B <: BNonZero, Q <: BNat](b: B)(implicit quotRem: BQuotRem.Aux[A, B, Q, _0_], snat: Q): quotRem.Quot = snat
  def %[B <: BNonZero, Q <: BNat, R <: BNat](b: B)(implicit quotRem: BQuotRem.Aux[A, B, Q, R], snat: R): quotRem.Rem = snat
}

trait BQuotSyntax {
  type #/#[X <: BNat, Y <: BNonZero] = BQuotRem[X, Y]
  type #\#[X <: BNat, Y <: BNonZero] = BQuotRem[X, Y] {type Rem = _0_}

  implicit def toQuotOps[A <: BNat](num: A): BQuotOps[A] = new BQuotOps[A](num)
}


