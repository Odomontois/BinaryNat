package com.awt.tree

import com.awt.tree.all._

import scala.collection.{Set, SetLike}
import scalaz._
import EitherT._

/**
  * User: Oleg
  * Date: 25-Oct-15
  * Time: 20:51
  */
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
