package com.awt.tree

import com.awt.tree.all._

import scala.collection.{Set, SetLike}
import scala.language.higherKinds
import scalaz._
import Id._
import EitherT._

/**
  * User: Oleg
  * Date: 25-Oct-15
  * Time: 20:51
  */
trait TreeSet[A, T[_]] {
  type Lower[_]
  type Higher[_]

  def lower: TreeSet[A, Lower]
  def higher: TreeSet[A, Higher]

  def foldable: Foldable[T]
  def insert: Insert[A, T[A]]
  def remove: Remove[A, T[A]]
}

trait TreeSetImpl {
  implicit def unitTreeSet[A: Order]: TreeSet[A, UnitT] = new TreeSet[A, UnitT] {
    type Lower[X] = Unit
    type Higher[X] = X

    def lower = this

    def higher = leafTreeSet[A]

    def foldable = implicitly[Foldable[UnitT]]

    def insert = implicitly[Insert[A, Unit]]

    def remove = implicitly[Remove[A, Unit]]
  }

  implicit def leafTreeSet[A: Order]: TreeSet[A, Id] = new TreeSet[A, Id] {
    type Lower[X] = Unit
    type Higher[X] = Tree23.Ind[Id, X]

    def foldable: Foldable[Id.Id] = implicitly[Foldable[Id]]

    def higher: TreeSet[A, Higher] = ???

    def insert: Insert[A, Id.Id[A]] = implicitly[Insert[A, A]]

    def remove: Remove[A, Id.Id[A]] = implicitly[Remove[A, A]]

    def lower: TreeSet[A, Lower] = unitTreeSet[A]
  }
}

class Set23[A, +T](elems: T)(implicit
                             remove: Remove[A, T],
                             insert: Insert[A, T]) {

  def +(elem: A): Set23[A, Any] = ???

  //    insert(elem, elems) match {
  //    case -\/(next) => new Set23(next)
  //    case \/-(next) => new Set23(next)
  //  }

  def contains(elem: A): Boolean = ???

  def -(elem: A): Set23[A, AnyRef] = ???

  def iterator: Iterator[A] = ???
}


object Set23 {
}
