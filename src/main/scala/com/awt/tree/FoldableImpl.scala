package com.awt.tree

import scala.language.higherKinds
import scalaz._
import scalaz.syntax.monoid._
import scalaz.syntax.foldable._

/**
  * User: Oleg
  * Date: 25-Oct-15
  * Time: 15:54
  */
trait FoldableImpl {
  type Tree23P[T[+ _]] = {type λ[+A] = Tree23[A, T[A]]}
  implicit def foldableTree[T[_]](implicit foldable: Foldable[T]) = new Foldable.FromFoldMap[Tree23P[T]#λ] {
    override def foldMap[A, B](fa: Tree23[A, T[A]])(f: (A) => B)(implicit F: Monoid[B]): B = fa match {
      case Tree2(ltree, rtree, _) => ltree.foldMap(f) ⊹ rtree.foldMap(f)
      case Tree3(ltree, mtree, rtree, _, _) => ltree.foldMap(f) ⊹ mtree.foldMap(f) ⊹ rtree.foldMap(f)
    }
  }
}
