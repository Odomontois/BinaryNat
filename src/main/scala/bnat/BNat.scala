/**
  * Author: Oleg Nizhnik
  * Date  : 22.10.2015
  * Time  : 15:03
  */
package bnat

import BNat._

import scala.language.{higherKinds, implicitConversions}

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
  final type _0_ = BZero.type

  final type _1_ = BOdd[BZero.type]

  final type _0[X <: BNonZero] = BEven[X]

  final type _1[X <: BNonZero] = BOdd[X]

  final type ##[X <: BNonZero, F[_X <: BNonZero]] = F[X]

  implicit val _0_ = BZero

  val _1_ = BOdd(BZero)

  def _1[X <: BNonZero]: X ⇒ BOdd[X] = BOdd(_)
  def _0[X <: BNonZero]: X ⇒ BEven[X] = BEven(_)
}

trait BnatImpl {
  implicit def constructEven[X <: BNonZero](implicit prev: X) = BEven(prev)
  implicit def constructOdd[X <: BNat](implicit prev: X) = BOdd(prev)
}

class BNonZeroOps[X <: BNonZero](val x: X) extends AnyVal {
  def _1 = BOdd(x)

  def _0 = BEven(x)

  def _1[Y <: BNat](d: BOdd[X] ⇒ Y): Y = d(BOdd(x))

  def _0[Y <: BNat](d: BEven[X] ⇒ Y): Y = d(BEven(x))
}

trait BNatSyntax extends BNatTypes {
  type BNonZero = BSucc[_]

  type BZero = BZero.type

  implicit def toBNonZeroOps[N <: BNonZero](x: N): BNonZeroOps[N] = new BNonZeroOps(x)
}

object BNat extends BnatImpl


