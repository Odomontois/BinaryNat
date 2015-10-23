/**
 * Author: Oleg Nizhnik
 * Date  : 22.10.2015
 * Time  : 15:03
 */
package com.awt.bnat
import BNat._

import scala.language.implicitConversions

sealed trait BNat {
  def toBigInt: BigInt
}

sealed trait BSucc[T <: BNat] extends BNat

case object BZero extends BNat {
  val toBigInt = implicitly[Integral[BigInt]].zero
}

case class BEven[T <: BNonZero](prev: T) extends BSucc[T] {
  def toBigInt = prev.toBigInt << 1
}

case class BOdd[T <: BNat](prev: T) extends BSucc[T] {
  def toBigInt = (prev.toBigInt << 1) + 1
}
trait BNatTypes {
  final type _0 = BZero.type

  final type _1 = BOdd[BZero.type]

  sealed trait ##[P <: BNonZero, D <: BNat] {
    type Out <: BNat
    def nat: Out
  }

  implicit val _0 = BZero

  val _1 = BOdd(BZero)

  implicit def hashConstructEven[X <: BNonZero](implicit prev: X) = new (X ## _0)() {
    type Out = BEven[X]
    val nat: Out = BEven(prev)
  }
  implicit def hashConstructOdd[X <: BNonZero](implicit prev: X) = new (X ## _1)() {
    type Out = BOdd[X]
    val nat: Out = BOdd(prev)
  }

  implicit def constructEven[X <: BNonZero](implicit prev: X) = BEven(prev)
  implicit def constructOdd[X <: BNat](implicit prev: X) = BOdd(prev)
}

class BNonZeroOps[N <: BNonZero](val n: N) extends AnyVal {
  def ##[D <: BNat](d: D)(implicit nat: (N ## D)): nat.Out = nat.nat

  def _1 = BOdd(n)

  def _0 = BEven(n)

  def _1[D <: BNat](d: D)(implicit nat: (BOdd[N] ## D)): nat.Out = nat.nat

  def _0[D <: BNat](d: D)(implicit nat: (BEven[N] ## D)): nat.Out = nat.nat
}

trait BNatAll extends BNatTypes {
  type BNonZero = BSucc[_]

  type BZero = BZero.type

  implicit def toBNonZeroOps[N <: BNonZero](x: N): BNonZeroOps[N] = new BNonZeroOps(x)
}

object BNat extends BNatAll


