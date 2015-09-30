package com.awt.binnat

/**
 * Author: Oleg Nizhnik
 * Date  : 29.09.2015
 * Time  : 9:27
 */
import shapeless.Witness

sealed trait BNat {
  def asBigInt: BigInt

  def repr: List[BDigit]

  def length: Int

  override def toString = repr.reverse.map(_.asInt).mkString("bn", "", "")
}

trait BNatTypes {
  final type _0 = BZero.type

  final type _1 = BOne.type

  final type ##[P <: BNonZero, D <: BDigit] = BSucc[D, P]

  final implicit val _0 = BZero

  final implicit val _1 = BOne

  final implicit def bSuccWit[D <: BDigit, P <: BNonZero]
  (implicit witD: Witness.Aux[D], witP: Witness.Aux[P]) = new Witness {
    type T = BSucc[D, P]
    val value = new BSucc[D, P](witD.value, witP.value)
  }

  final implicit def bSucc[D <: BDigit, P <: BNonZero]
  (implicit d: D, p: P) = new BSucc[D, P](d, p)

}

sealed trait BNonZero extends BNat

sealed class BDigit(val asInt: Int) extends BNat {
  val length = 1

  val asBigInt = BigInt(asInt)

  val repr = List(this)
}

object BZero extends BDigit(0)

object BOne extends BDigit(1) with BNonZero

class BSucc[D <: BDigit, P <: BNonZero](d: D, p: P) extends BNonZero {
  lazy val asBigInt = p.asBigInt * 2 + d.asBigInt

  lazy val repr = d :: p.repr

  val length = 1 + p.length
}
