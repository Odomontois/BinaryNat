/**
 * Author: Oleg Nizhnik
 * Date  : 22.10.2015
 * Time  : 15:11
 */
package bnat

import scala.language.higherKinds
import scalaz.Ordering
import Ordering._

sealed trait BCompare[X <: BNat, Y <: BNat] {
  type Result <: Ordering
  def result: Result
}
sealed abstract class BCompareA[X <: BNat, Y <: BNat, O <: Ordering](val result: O) extends BCompare[X, Y] {
  type Result = O
}

class #<#[X <: BNat, Y <: BNat] private[bnat]() extends BCompareA[X, Y, LT](LT)
class #>#[X <: BNat, Y <: BNat] private[bnat]() extends BCompareA[X, Y, GT](GT)
class #=#[X <: BNat, Y <: BNat] private[bnat]() extends BCompareA[X, Y, EQ](EQ)

trait BCompareLowLevel {
  implicit def bLessReverseIsMore[X <: BNat, Y <: BNat](implicit lt: X #<# Y) = new (Y #># X)()
  implicit def equailityIsTypeEquality[X <: BNat, Y <: BNat](implicit eq: X =:= Y) = new (X #=# Y)()
}

trait BCompareImpl extends BCompareLowLevel {
  implicit def _0_lt_nz[X <: BNonZero] = new (BZero #<# X)()
  implicit def _even_lt_odd[X <: BNonZero] = new (BEven[X] #<# BOdd[X])
  implicit def _p0_lt_p0[X <: BNonZero, Y <: BNonZero](implicit lt: X #<# Y) = new (BEven[X] #<# BEven[Y])()
  implicit def _p0_lt_p1[X <: BNonZero, Y <: BNat](implicit lt: X #<# Y) = new (BEven[X] #<# BOdd[Y])()
  implicit def _p1_lt_p0[X <: BNat, Y <: BNonZero](implicit lt: X #<# Y) = new (BOdd[X] #<# BEven[Y])()
  implicit def _p1_lt_p1[X <: BNat, Y <: BNat](implicit lt: X #<# Y) = new (BOdd[X] #<# BOdd[Y])()
}


trait BCompareSyntax{
  final type LT = Ordering.LT.type
  final type GT = Ordering.GT.type
  final type EQ = Ordering.EQ.type
  final type #?#[X <: BNat, Y <: BNat] = BCompare[X, Y]

  final implicit def bOrder[X <: BNat, Y <: BNat](x: X, y: Y)
                                           (implicit bCompare: BCompare[X, Y]): bCompare.Result = bCompare.result
}
object BCompare extends BCompareImpl