/**
 * Author: Oleg Nizhnik
 * Date  : 23.10.2015
 * Time  : 12:52
 */
package bnat

import BNat._

import scala.language.implicitConversions

sealed trait BMinus[A <: BNat, B <: BNat] {
  type Out <: BNat
}

sealed trait BMinusNZ[A <: BNonZero, B <: BNat] extends BMinus[A, B] {
  type Out <: BNonZero
}

sealed trait BMinusM1[A <: BNonZero, B <: BNat] {
  type Out <: BNat
}

sealed trait BMinusM1NZ[A <: BNonZero, B <: BNat] extends BMinusM1[A, B] {
  type Out <: BNonZero
}

sealed trait BMinusImpl extends BMinusM1Impl{
  implicit def _nz_minus_0[X <: BNonZero] = new BMinusNZ[X, _0_] {type Out = X}
  implicit def _x_minus_x[X <: BNat] = new BMinus[X, X] {type Out = _0_}

  implicit def _even_minus_even[X <: BNonZero, Y <: BNonZero](implicit minus: BMinusNZ[X, Y]) =
    new BMinusNZ[BEven[X], BEven[Y]] {type Out = BEven[minus.Out]}
  implicit def _even_minus_odd[X <: BNonZero, Y <: BNat](implicit minus: BMinusM1[X, Y]) =
    new BMinusNZ[BEven[X], BOdd[Y]] {type Out = BOdd[minus.Out]}
  implicit def _odd_minus_even[X <: BNat, Y <: BNonZero](implicit minus: BMinus[X, Y]) =
    new BMinusNZ[BOdd[X], BEven[Y]] {type Out = BOdd[minus.Out]}
  implicit def _odd_minus_odd[X <: BNonZero, Y <: BNat](implicit minus: BMinusNZ[X, Y]) =
    new BMinusNZ[BOdd[X], BOdd[Y]] {type Out = BEven[minus.Out]}
}

sealed trait BMinusM1Impl {
  implicit object _one_minus_0_m1 extends BMinusM1[_1_, _0_] {type Out = _0_}
  implicit def _2xp1_minus_2x_m1[X <: BNonZero] =
    new BMinusM1[BOdd[X], BEven[X]] {type Out = _0_}

  implicit def _even_minus_0_m1[X <: BNonZero](implicit minus: BMinusM1[X, _0_]) =
    new BMinusM1NZ[BEven[X], _0_] {type Out = BOdd[minus.Out]}
  implicit def _odd_minus_0_m1[X <: BNonZero] =
    new BMinusM1NZ[BOdd[X], _0_] {type Out = BEven[X]}

  implicit def _even_minus_even_m1[X <: BNonZero, Y <: BNonZero](implicit minus: BMinusM1[X, Y]) =
    new BMinusM1NZ[BEven[X], BEven[Y]] {type Out = BOdd[minus.Out]}
  implicit def _even_minus_odd_m1[X <: BNonZero, Y <: BNat](implicit minus: BMinusM1NZ[X, Y]) =
    new BMinusM1NZ[BEven[X], BOdd[Y]] {type Out = BEven[minus.Out]}
  implicit def _odd_minus_even_m1[X <: BNonZero, Y <: BNonZero](implicit minus: BMinusNZ[X, Y]) =
    new BMinusM1NZ[BOdd[X], BEven[Y]] {type Out = BEven[minus.Out]}
  implicit def _odd_minus_odd_m1[X <: BNonZero, Y <: BNat](implicit minus: BMinusM1[X, Y]) =
    new BMinusM1NZ[BOdd[X], BOdd[Y]] {type Out = BOdd[minus.Out]}
}

class BMinusOps[X <: BNat](val x: X) extends AnyVal {
  def -[Y <: BNat, D <: BNat](y: Y)(implicit diff: BMinus[X, Y] {type Out = D}, nat: D) = nat
}

trait BMinusSyntax {
  type #-#[X <: BNat, Y <: BNat] = BMinus[X,Y]

  @inline implicit def toMinusOps[X <: BNat](x: X): BMinusOps[X] = new BMinusOps[X](x)
}

object BMinus extends BMinusImpl
