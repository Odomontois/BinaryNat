/**
 * Author: Oleg Nizhnik
 * Date  : 29.09.2015
 * Time  : 13:29
 */
package com.awt.binnat
import scala.language.implicitConversions

sealed trait #<#[X <: BNat, Y <: BNat]

trait BLessImpl {
  implicit def _0_lt_nz[N <: BNonZero] = new (_0 #<# N) {}
  implicit def _1_lt_n[N <: BNonZero, D <: BDigit] = new (_1 #<# (N ## D)) {}
  implicit def _n0_lt_n1[N <: BNonZero] = new ((N ## _0) #<# (N ## _1)) {}
  implicit def _n_lt_m[Na <: BNonZero, Nb <: BNonZero, Da <: BDigit, Db <: BDigit](implicit lt: Na #<# Nb) =
    new ((Na ## Da) #<# (Nb ## Db)) {}
}

