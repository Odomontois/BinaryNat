package com.awt

/**
 * Author: Oleg Nizhnik
 * Date  : 29.09.2015
 * Time  : 9:27
 */
import com.awt.BNat._

sealed trait BNat {
  def asBigInt: BigInt

  def repr: List[BDigit]

  def length: Int
}

object BNat {
  type _0 = BZero.type

  type _1 = BOne.type

  type ##[P <: BNonZero, D <: BDigit] = BSucc[D, P]

  implicit val _0 = BZero

  implicit val _1 = BOne

  implicit def bSuccWit[D <: BDigit, P <: BNonZero]
  (implicit witD: D, witP: P) = BSucc[D, P](witD, witP)

  implicit class BNonZeroOps[N <: BNonZero](val n: N) extends AnyVal {
    def ##[D <: BDigit](d: D): N ## D = BSucc[D, N](d, n)

    def _1 = ##(BOne)

    def _0 = ##(BZero)

    def _1[D <: BDigit](d: D) = ##(BOne) ## d

    def _0[D <: BDigit](d: D) = ##(BZero) ## d
  }
}

sealed trait BNonZero extends BNat

sealed class BDigit(val asInt: Int) extends BNat {
  val length = 1

  val asBigInt = BigInt(asInt)

  val repr = List(this)
}

case object BZero extends BDigit(0)

case object BOne extends BDigit(1) with BNonZero

case class BSucc[D <: BDigit, P <: BNonZero](d: D, p: P) extends BNonZero {
  lazy val asBigInt = p.asBigInt * 2 + d.asBigInt

  lazy val repr = d.repr ++ p.repr

  val length = 1 + p.length
}

sealed trait #<#[X <: BNat, Y <: BNat]

trait BLessImpl {
  implicit def _0_lt_nz[N <: BNonZero] = new (_0 #<# N) {}
  implicit def _1_lt_n[N <: BNonZero, D <: BDigit] = new (_1 #<# (N ## D)) {}
  implicit def _n0_lt_n1[N <: BNonZero] = new ((N ## _0) #<# (N ## _1)) {}
}

sealed trait #+#[X <: BNat, Y <: BNat] {
  type Out <: BNat
}

object #+# {
  type Aux[X <: BNat, Y <: BNat, X_plus_Y <: BNat] = #+#[X, Y] {type Out = X_plus_Y}
}

trait BSumImpl {
  import BNat._
  implicit object _0_plus_0 extends (_0 #+# _0) {
    type Out = _0
  }
  implicit object _0_plus_1 extends (_0 #+# _1) {
    type Out = _1
  }
  implicit object _1_plus_0 extends (_1 #+# _0) {
    type Out = _1
  }
  implicit object _1_plus_1 extends (_1 #+# _1) {
    type Out = _1 ## _0
  }

  implicit def _0_plus_n[N <: BNonZero] =
    new (_0 #+# N) {type Out = N}

  implicit def _1_plus_n0[N <: BNonZero] =
    new (_1 #+# (N ## _0)) {type Out = N ## _1}

  implicit def _1_plus_n1[N <: BNonZero, LOut <: BNonZero](implicit lesser: #+#.Aux[_1, N, LOut]) =
    new (_1 #+# (N ## _1)) {type Out = lesser.Out ## _0}
}

