package com.awt.binnat

/**
 * Author: Oleg Nizhnik
 * Date  : 29.09.2015
 * Time  : 9:27
 */
import shapeless.Poly2

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
  (implicit witD: D, witP: P) = new BSucc[D, P](witD, witP)

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

  lazy val repr = d.repr ++ p.repr

  val length = 1 + p.length
}
