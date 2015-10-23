/**
 * Author: Oleg Nizhnik
 * Date  : 23.10.2015
 * Time  : 10:47
 */
package com.awt.bnat
import BPlus._
import BNat._

import scala.language.implicitConversions

sealed trait BPlus[X <: BNat, Y <: BNat] {
  type Out <: BNat
}

sealed trait BPlusNZ[X <: BNat, Y <: BNat] extends BPlus[X, Y] {
  type Out <: BNonZero
}

sealed trait BPlusM1[X <: BNat, Y <: BNat] {
  type Out <: BNonZero
}

trait BPlusImpl {
  implicit object _0_plus_0 extends BPlus[_0, _0] {type Out = _0}
  implicit def _0_plus_nz[X <: BNonZero] = new BPlusNZ[_0, X] {type Out = X}
  implicit def _nz_plus_0[X <: BNonZero] = new BPlusNZ[X, _0] {type Out = X}

  implicit def _even_plus_even[X <: BNonZero, Y <: BNonZero](implicit sum: BPlusNZ[X, Y]) =
    new BPlusNZ[BEven[X], BEven[Y]] {type Out = BEven[sum.Out]}
  implicit def _even_plus_odd[X <: BNonZero, Y <: BNat](implicit sum: BPlus[X, Y]) =
    new BPlusNZ[BEven[X], BOdd[Y]] {type Out = BOdd[sum.Out]}
  implicit def _odd_plus_even[X <: BNat, Y <: BNonZero](implicit sum: BPlus[X, Y]) =
    new BPlusNZ[BOdd[X], BEven[Y]] {type Out = BOdd[sum.Out]}
  implicit def _odd_plus_odd[X <: BNat, Y <: BNat](implicit sum1: BPlusM1[X, Y]) =
    new BPlusNZ[BOdd[X], BOdd[Y]] {type Out = BEven[sum1.Out]}

}

trait BPlusM1Impl {
  implicit object _0_plus_0_m1 extends BPlusM1[_0, _0] {type Out = _1}

  implicit def _0_plus_even_m1[X <: BNonZero] =
    new BPlusM1[_0, BEven[X]] {type Out = BOdd[X]}
  implicit def _even_plus_0_m1[X <: BNonZero] =
    new BPlusM1[BEven[X], _0] {type Out = BOdd[X]}
  implicit def _0_plus_odd_m1[X <: BNat](implicit next: BPlusM1[_0, X]) =
    new BPlusM1[_0, BOdd[X]] {type Out = BEven[next.Out]}
  implicit def _odd_plus_0_m1[X <: BNat](implicit next: BPlusM1[_0, X]) =
    new BPlusM1[BOdd[X], _0] {type Out = BEven[next.Out]}

  implicit def _even_plus_even_m1[X <: BNonZero, Y <: BNonZero](implicit sum: BPlus[X, Y]) =
    new BPlusM1[BEven[X], BEven[Y]] {type Out = BOdd[sum.Out]}
  implicit def _even_plus_odd_m1[X <: BNonZero, Y <: BNat](implicit sum1: BPlusM1[X, Y]) =
    new BPlusM1[BEven[X], BOdd[Y]] {type Out = BEven[sum1.Out]}
  implicit def _odd_plus_even_m1[X <: BNat, Y <: BNonZero](implicit sum1: BPlusM1[X, Y]) =
    new BPlusM1[BOdd[X], BEven[Y]] {type Out = BEven[sum1.Out]}
  implicit def _odd_plus_odd_m1[X <: BNat, Y <: BNat](implicit sum1: BPlusM1[X, Y]) =
    new BPlusM1[BOdd[X], BOdd[Y]] {type Out = BOdd[sum1.Out]}
}

class BPlusOps[X <: BNat](val x: X) extends AnyVal {
  def +[Y <: BNat, S <: BNat](y: Y)(implicit sum: BPlus[X, Y] {type Out = S}, snat: S) = snat
}

trait BPlusAll extends BPlusImpl with BPlusM1Impl {
  type #+#[X <: BNat, Y <: BNat] = BPlus[X, Y]

  @inline implicit def makeBplusOps[X <: BNat](x: X): BPlusOps[X] = new BPlusOps(x)
}

object BPlus extends BPlusAll
