package com.awt.tree

import com.awt.tree.all._

import scala.collection.{Set, SetLike}
import scala.language.higherKinds
import scalaz._
import Id._
import EitherT._
/**
  * U N S U C C E S S F U L
  */

/**
  * User: Oleg
  * Date: 25-Oct-15
  * Time: 20:51
//  */
//trait TreeSet[A, That[_]] {
//  type Low[_]
//  type High[_]
//  type T = That[A]
//
//  def lower: TreeSet[A, Low] {
//    type High[X] = That[X]
//  }
//  def higher: TreeSet[A, High]{
//    type Low[X] = That[X]
//  }
//
//  def foldable: Foldable[That]
//  def insert: Insert.Aux[A, T, High[A]]
//  def remove: Remove[A, T]
//}
//
//object TreeSet {
//  type Aux[A, F[_], H[_]] = TreeSet[A, F] {type High[X] = H[X]}
//}
//
//trait TreeSetImpl {
//  implicit def unitTreeSet[A: Order]: TreeSet[A, UnitT] = new TreeSet[A, UnitT] {
//    type Low[X] = Unit
//    type High[X] = X
//
//    def lower = this
//
//    def higher = leafTreeSet[A]
//
//    def foldable = implicitly[Foldable[UnitT]]
//
//    def insert = insertUnit
//
//    def remove = removeUnit
//  }
//
//  implicit def leafTreeSet[A: Order]: TreeSet[A, Id] = new TreeSet[A, Id] {
//    type Low[X] = Unit
//    type High[X] = Tree23[X, X]
//    type That[X] = X
//
//    def foldable: Foldable[Id.Id] = implicitly[Foldable[Id]]
//
//    def insert: Insert[A, Id.Id[A]] = implicitly[Insert[A, A]]
//
//    def remove: Remove[A, Id.Id[A]] = implicitly[Remove[A, A]]
//
//    def higher: TreeSet[A, High] = bottomTreeSet[A]
//
//    def lower: TreeSet[A, Low] = unitTreeSet[A]
//  }
//
//  implicit def bottomTreeSet[A: Order]: TreeSet[A, λ[X => Tree23[X, X]]] = new TreeSet[A, λ[X => Tree23[X, X]]] {
//    type Low[X] = X
//    type High[X] = Tree23[X, Tree23[X, X]]
//    type That[X] = Tree23[X, X]
//
//    def foldable: Foldable[That] = foldableTree[Id]
//
//    def insert: Insert[A, Tree23[A, A]] = implicitly[Insert[A, That[A]]]
//    def remove: Remove[A, Tree23[A, A]] = implicitly[Remove[A, That[A]]]
//
//    def lower: TreeSet[A, Low] = leafTreeSet[A]
//
//    def higher: TreeSet[A, High] = {
//      implicit val prev = this
//      nodeTreeSet[A, That]
//    }
//  }
//
//  implicit def nodeTreeSet[A: Order, F[_]](implicit lower: TreeSet.Aux[A, F, λ[X => Tree23[X, F[X]]]]): TreeSet[A, λ[X => Tree23[X, F[X]]]] =
//    new TreeSet[A, λ[X => Tree23[X, F[X]]]] {
//      type Low[X] = F[X]
//      type High[X] = Tree23[X, Tree23[X, F[X]]]
//      type That[X] = Tree23[X, F[X]]
//
//      def foldable: Foldable[That] = {
//        implicit val low: Foldable[F] = lower.foldable
//        foldableTree[F]
//      }
//
//      def insert: Insert[A, Tree23[A, F[A]]] = {
//        implicit val low: Insert[A, F[A]]{type Higher = Tree23[A, F[A]]} = lower.insert
//        insertNode[A, F[A]]
//      }
//
//      def remove: Remove[A, Tree23[A, F[A]]] = ???
//
//      def lower: TreeSet[A, Low] = ???
//      def higher: TreeSet[A, High] = ???
//    }
//}
//
//class Set23[A, +T](elems: T)(implicit
//                             remove: Remove[A, T],
//                             insert: Insert[A, T]) {
//
//  def +(elem: A): Set23[A, Any] = ???
//
//  //    insert(elem, elems) match {
//  //    case -\/(next) => new Set23(next)
//  //    case \/-(next) => new Set23(next)
//  //  }
//
//  def contains(elem: A): Boolean = ???
//
//  def -(elem: A): Set23[A, AnyRef] = ???
//
//  def iterator: Iterator[A] = ???
//}

object Set23 {
}
