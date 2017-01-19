package bnat

import BNat._

import scala.language.implicitConversions

/**
  * User: Oleg
  * Date: 24-Oct-15
  * Time: 13:02
  */
sealed trait BTimes[X <: BNat, Y <: BNat] {
  type Out <: BNat
}

sealed trait BTimesNZ[X <: BNonZero, Y <: BNonZero] extends BTimes[X, Y] {
  type Out <: BNonZero
}

trait BTimesImpl {
  implicit def _0_times_x[X <: BNat] = new BTimes[_0_, X] {type Out = _0_}

  implicit def _nz_times_0_[X <: BNonZero] = new BTimes[X, _0_] {type Out = _0_}

  implicit def _nz_times_even[X <: BNonZero, Y <: BNonZero](implicit times: BTimesNZ[X, Y]) =
    new BTimesNZ[X, BEven[Y]] {type Out = BEven[times.Out]}

  implicit def _nz_times_1[X <: BNonZero] = new BTimesNZ[X, _1_] {type Out = X}

  implicit def _nz_times_odd[X <: BNonZero, Y <: BNonZero, P <: BNonZero](implicit times: BTimesNZ[X, Y] {type Out = P}, plus: BPlusNZ[BEven[P], X]) =
    new BTimesNZ[X, BOdd[Y]] {type Out = plus.Out}
}

object BTimes extends BTimesImpl

class BTimesOps[X <: BNat](val x: X) extends AnyVal {
  def *[Y <: BNat, P <: BNat](y: Y)(implicit times: BTimes[X, Y] {type Out = P}, p: P) = p
}

trait BTimesSyntax{
  type #*#[X <: BNat, Y <: BNat] = BTimes[X, Y]

  implicit def toMulOps[X <: BNat](x: X): BTimesOps[X] = new BTimesOps(x)
}
