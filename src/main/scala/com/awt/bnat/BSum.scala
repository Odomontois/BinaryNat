/**
 * Author: Oleg Nizhnik
 * Date  : 23.10.2015
 * Time  : 10:47
 */
package com.awt.bnat
import BSum._
import BNat._

sealed trait BSum[X <: BNat, Y <: BNat] {
  type Out <: BNat
}

sealed trait BSumNZ[X <: BNat, Y <: BNat] extends BSum[X, Y] {
  type Out <: BNonZero
}

sealed trait BSumM1[X <: BNat, Y <: BNat] {
  type Out <: BNonZero
}

trait BSumImpl {
  implicit object _0_plus_0 extends BSum[_0, _0] {type Out = _0}
  implicit def _0_plus_nz[X <: BNonZero] = new BSumNZ[_0, X] {type Out = X}
  implicit def _nz_plus_0[X <: BNonZero] = new BSumNZ[X, _0] {type Out = X}

  implicit def _even_plus_even[X <: BNonZero, Y <: BNonZero](implicit sum: BSumNZ[X, Y]) =
    new BSumNZ[BEven[X], BEven[Y]] {type Out = BEven[sum.Out]}
  implicit def _even_plus_odd[X <: BNonZero, Y <: BNat](implicit sum: X #+# Y) =
    new BSumNZ[BEven[X], BOdd[Y]] {type Out = BOdd[sum.Out]}
  implicit def _odd_plus_even[X <: BNat, Y <: BNonZero](implicit sum: X #+# Y) =
    new BSumNZ[BOdd[X], BEven[Y]] {type Out = BOdd[sum.Out]}
  implicit def _odd_plus_odd[X <: BNat, Y <: BNat](implicit sum1: BSumM1[X, Y]) =
    new BSumNZ[BOdd[X], BOdd[Y]] {type Out = BEven[sum1.Out]}

}

trait BSumM1Impl {
  implicit object _0_plus_0_m1 extends BSumM1[_0, _0] {type Out = _1}

  implicit def _0_plus_even_m1[X <: BNonZero] =
    new BSumM1[_0, BEven[X]] {type Out = BOdd[X]}
  implicit def _even_plus_0_m1[X <: BNonZero] =
    new BSumM1[BEven[X], _0] {type Out = BOdd[X]}
  implicit def _0_plus_odd_m1[X <: BNat](implicit next: BSumM1[_0, X]) =
    new BSumM1[_0, BOdd[X]] {type Out = BEven[next.Out]}
  implicit def _odd_plus_0_m1[X <: BNat](implicit next: BSumM1[_0, X]) =
    new BSumM1[BOdd[X], _0] {type Out = BEven[next.Out]}

  implicit def _even_plus_even_m1[X <: BNonZero, Y <: BNonZero](implicit sum: X #+# Y) =
    new BSumM1[BEven[X], BEven[Y]] {type Out = BOdd[sum.Out]}
  implicit def _even_plus_odd_m1[X <: BNonZero, Y <: BNat](implicit sum1: BSumM1[X, Y]) =
    new BSumM1[BEven[X], BOdd[Y]] {type Out = BEven[sum1.Out]}
  implicit def _odd_plus_even_m1[X <: BNat, Y <: BNonZero](implicit sum1: BSumM1[X, Y]) =
    new BSumM1[BOdd[X], BEven[Y]] {type Out = BEven[sum1.Out]}
  implicit def _odd_plus_odd_m1[X <: BNat, Y <: BNat](implicit sum1: BSumM1[X, Y]) =
    new BSumM1[BOdd[X], BOdd[Y]] {type Out = BOdd[sum1.Out]}
}

object BSum extends BSumImpl with BSumM1Impl {
  type #+#[X <: BNat, Y <: BNat] = BSum[X, Y]

  implicit class BSumOps[X <: BNat](val x: X) extends AnyVal {
    def +[Y <: BNat, S <: BNat](y: Y)(implicit sum: (X #+# Y) {type Out = S}, snat: S) = snat
  }
}
