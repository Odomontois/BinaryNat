/**
 * Author: Oleg Nizhnik
 * Date  : 29.09.2015
 * Time  : 13:29
 */
package com.awt.binnat
import scala.language.implicitConversions
import scalaz.Ordering
import BCompare._

sealed trait BCompare[X <: BNat, Y <: BNat, O <: Ordering]

sealed trait #<#[X <: BNat, Y <: BNat] extends BCompare[X, Y, LT]
sealed trait #>#[X <: BNat, Y <: BNat] extends BCompare[X, Y, GT]
sealed trait #=#[X <: BNat, Y <: BNat] extends BCompare[X, Y, EQ]

sealed trait Comp

trait BCompareLowLevel {
  implicit def bLessReverseIsMore[X <: BNat, Y <: BNat](implicit lt: X #<# Y) = new (Y #># X) {}
  implicit def equailityIsTypeEquality[X <: BNat, Y <: BNat](implicit eq: X =:= Y) = new (X #=# Y){}
}

trait BCompareImpl {
  implicit def _0_lt_nz[N <: BNonZero] = new (_0 #<# N) {}
  implicit def _1_lt_n[N <: BNonZero, D <: BDigit] = new (_1 #<# (N ## D)) {}
  implicit def _n0_lt_n1[N <: BNonZero] = new ((N ## _0) #<# (N ## _1)) {}
  implicit def _n_lt_m[Na <: BNonZero, Nb <: BNonZero, Da <: BDigit, Db <: BDigit](implicit lt: Na #<# Nb) =
    new ((Na ## Da) #<# (Nb ## Db)) {}
}

object BCompare {
  import scalaz.Ordering.{EQ, GT, LT}
  type EQ = EQ.type
  type GT = GT.type
  type LT = LT.type
}

