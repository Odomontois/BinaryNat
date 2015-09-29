/**
 * Author: Oleg Nizhnik
 * Date  : 29.09.2015
 * Time  : 13:30
 */
package com.awt.binnat

sealed trait #+#[X <: BNat, Y <: BNat] {
  type Out <: BNat
}

object #+# {
  type Aux[X <: BNat, Y <: BNat, X_plus_Y <: BNat] = #+#[X, Y] {type Out = X_plus_Y}

  def apply[X <: BNat, Y <: BNat, Sum <: BNat](x: X, y: Y)(implicit sum: #+#.Aux[X, Y, Sum], n: Sum) = n
}

sealed trait BNonZeroSum[X <: BNat, Y <: BNat] extends (X #+# Y) {
  type Out <: BNonZero
}

sealed trait BDigitSum[X <: BDigit, Y <: BDigit] extends (X #+# Y) {
  type Out <: BDigit
}

//sum of numbers with memoised 1
sealed trait BSumMem1[X <: BNat, Y <: BNat] {
  type Out <: BNonZero
}
sealed trait BNZDigSumMem1[X <: BDigit, Y <: BDigit] extends BSumMem1[X, Y] {
  type Digit <: BDigit
  type Out = _1 ## Digit
}

object BSumMem1 {
  type Aux[X <: BNat, Y <: BNat, O] = BSumMem1[X, Y] {type Out = O}
}

trait BSumLowLevelImpl {
  implicit def nonZeroSumCommute[X <: BNonZero, Y <: BNonZero]
  (implicit sum: BNonZeroSum[Y, X], lt: Y #<# X) =
    new BNonZeroSum[X, Y] {type Out = sum.Out}

  implicit def sumMem1Commute[X <: BSucc[_, _], Y <: BSucc[_, _]]
  (implicit sum: BSumMem1[Y, X], lt: Y #<# X) =
    new BSumMem1[X, Y] {type Out = sum.Out}
}

trait BSumImpl extends BSumLowLevelImpl with BSumMem1Impl {

  implicit object _0_plus_0 extends BDigitSum[_0, _0] {
    type Out = _0
  }
  implicit object _0_plus_1 extends BDigitSum[_0, _1] with BNonZeroSum[_0, _1] {
    type Out = _1
  }

  implicit object _1_plus_0 extends BDigitSum[_1, _0] with BNonZeroSum[_1, _0] {
    type Out = _1
  }

  implicit object _1_plus_1 extends BNonZeroSum[_1, _1] {
    type Out = _1 ## _0
  }

  implicit def _0_plus_n[N <: BNonZero, D <: BDigit] =
    new BNonZeroSum[_0, N ## D] {type Out = N ## D}

  implicit def _n_plus_0[N <: BNonZero, D <: BDigit] =
    new BNonZeroSum[N ## D, _0] {type Out = N ## D}

  implicit def _1_plus_n0[N <: BNonZero] =
    new BNonZeroSum[_1, N ## _0] {type Out = N ## _1}

  implicit def _1_plus_n1[N <: BNonZero, LOut <: BNonZero](implicit higher: #+#.Aux[_1, N, LOut]) =
    new BNonZeroSum[_1, N ## _1] {type Out = higher.Out ## _0}

  implicit def _n_plus_m_digit[Na <: BNonZero, Nb <: BNonZero, Da <: BDigit, Db <: BDigit]
  (implicit digitSum: BDigitSum[Da, Db], higher: BNonZeroSum[Na, Nb]) =
    new BNonZeroSum[Na ## Da, Nb ## Db] {type Out = higher.Out ## digitSum.Out}

  implicit def _n1_plus_m1[Na <: BNonZero, Nb <: BNonZero]
  (implicit higher: BSumMem1[Na, Nb]) =
    new BNonZeroSum[Na ## _1, Nb ## _1] {type Out = higher.Out ## _0}

}

trait BSumMem1Impl {
  implicit object _0_plus_0_mem1 extends BSumMem1[_0, _0] {
    type Out = _1
  }

  implicit object _0_plus_1_mem1 extends BNZDigSumMem1[_0, _1] {
    type Digit = _0
  }

  implicit object _1plus_0_mem1 extends BNZDigSumMem1[_1, _0] {
    type Digit = _0
  }

  implicit object _1_plus_1_mem1 extends BNZDigSumMem1[_1, _1] {
    type Digit = _1
  }

  implicit def _0_plus_n0_mem1[N <: BNonZero] =
    new BSumMem1[_0, N ## _0] {type Out = N ## _1}

  implicit def _0_plus_n1_mem1[N <: BNonZero](implicit higher: BNonZeroSum[_1, N]) =
    new BSumMem1[_0, N ## _1] {type Out = higher.Out ## _1}

  implicit def _1_plus_n_mem1[N <: BNonZero, D <: BDigit](implicit higher: BNonZeroSum[_1, N]) =
    new BSumMem1[_1, N ## D] {type Out = higher.Out ## D}

  implicit def _n0_plus_m0[N <: BNonZero, M <: BNonZero](implicit higher: BNonZeroSum[N, M]) =
    new BSumMem1[N ## _0, M ## _0] {type Out = higher.Out ## _1}

  implicit def _n_plus_m[Na <: BNonZero, Nb <: BNonZero, Da <: BDigit, Db <: BDigit, Ds <: BDigit]
  (implicit higher: BSumMem1[Na, Nb], digitSum: BNZDigSumMem1[Da, Db]) =
    new BSumMem1[Na ## Da, Nb ## Db] {type Out = higher.Out ## digitSum.Digit}
}
