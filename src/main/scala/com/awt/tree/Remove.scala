package com.awt.tree

import scalaz.Ordering._
import scalaz._
import scalaz.syntax.order._
import scalaz.syntax.either._

/**
  * User: Oleg
  * Date: 25-Oct-15
  * Time: 16:49
  */
trait Remove[A, B] {
  type Lower
  def apply(elem: A, tree: B): B \/ Lower
}

object Remove {
  def apply[A, B, L](remove: (A, B) => B \/ L) = new Remove[A, B] {
    type Lower = L

    def apply(elem: A, tree: B) = remove(elem, tree)
  }
}

trait RemoveImpl {

  def remove[A, T](a: A, t: T)(implicit r: Remove[A, T]): T \/ r.Lower = r(a, t)

  implicit def removeUnit[A] = Remove[A, Unit, Unit] { (elem, _unit) => ().left }

  implicit def removeLeaf[A: Equal] = Remove[A, A, Unit] { (elem, tree) =>
    if (elem === tree) ().right else tree.left
  }

  implicit def removeBottom[A: Order](implicit lower: Remove[A, A]) = Remove[A, Tree23[A, A], A] { (elem, tree) => tree match {
    case Tree2(ltree, rtree, sep) => elem cmp sep match {
      case LT => lower(elem, ltree) match {
        case _saved: -\/[_] => tree.left
        case _deleted: \/-[_] => rtree.right
      }
      case EQ | GT => lower(elem, rtree) match {
        case _saved: -\/[_] => tree.left
        case _deleted: \/-[_] => ltree.right
      }
    }
    case Tree3(ltree, mtree, rtree, lsep, rsep) =>
      elem cmp lsep match {
        case LT => lower(elem, ltree) match {
          case _saved: -\/[_] => tree.left
          case _deleted: \/-[_] => Tree2(mtree, rtree, rsep).left
        }
        case EQ | GT => elem cmp rsep match {
          case LT => lower(elem, mtree) match {
            case _saved: -\/[_] => tree.left
            case _deleted: \/-[_] => Tree2(ltree, rtree, rsep).left
          }
          case EQ | GT => lower(elem, rtree) match {
            case _saved: -\/[_] => tree.left
            case _deleted: \/-[_] => Tree2(ltree, mtree, lsep).left
          }
        }
      }
  }
  }

  implicit def removeNode[A: Order, B](implicit lower: Remove[A, Tree23[A, B]] {type Lower = B}) =
    Remove[A, Tree23[A, Tree23[A, B]], Tree23[A, B]] { (elem, tree) => tree match {
      case Tree2(ltree, rtree, sep) => elem cmp sep match {
        case LT => lower(elem, ltree) match {
          case -\/(lnew) => Tree2(lnew, rtree, sep).left
          case \/-(lnew) => rtree match {
            case Tree2(llow, rlow, rsep) => Tree3(lnew, llow, rlow, sep, rsep).right
            case Tree3(llow, mlow, rlow, lsep, rsep) => Tree2(Tree2(lnew, llow, sep), Tree2(mlow, rlow, rsep), lsep).left
          }
        }
        case EQ | GT => lower(elem, rtree) match {
          case -\/(rnew) => Tree2(ltree, rnew, sep).left
          case \/-(rnew) => ltree match {
            case Tree2(llow, rlow, lsep) => Tree3(llow, rlow, rnew, lsep, sep).right
            case Tree3(llow, mlow, rlow, lsep, rsep) => Tree2(Tree2(llow, mlow, lsep), Tree2(rlow, rnew, sep), rsep).left
          }
        }
      }
      case Tree3(ltree, mtree, rtree, lsep, rsep) => elem cmp lsep match {
        case LT => lower(elem, ltree) match {
          case -\/(lnew) => Tree3(lnew, mtree, rtree, lsep, rsep).left
          case \/-(lnew) => mtree match {
            case Tree2(llow, rlow, lowsep) => Tree2(Tree3(lnew, llow, rlow, lsep, lowsep), rtree, rsep).left
            case Tree3(llow, mlow, rlow, mlsep, mrsep) => Tree3(Tree2(lnew, llow, lsep), Tree2(mlow, rlow, mrsep), rtree, mlsep, rsep).left
          }
        }
        case EQ | GT => elem cmp rsep match {
          case LT => lower(elem, mtree) match {
            case -\/(mnew) => Tree3(ltree, mnew, rtree, lsep, rsep).left
            case \/-(mnew) => ltree match {
              case Tree2(llow, rlow, lowsep) => Tree2(Tree3(llow, rlow, mnew, lowsep, lsep), rtree, lsep).left
              case Tree3(llow, mlow, rlow, llsep, lrsep) => Tree3(Tree2(llow, mlow, llsep), Tree2(rlow, mnew, lsep), rtree, lrsep, rsep).left
            }
          }
          case EQ | GT => lower(elem, rtree) match {
            case -\/(rnew) => Tree3(ltree, mtree, rnew, lsep, rsep).left
            case \/-(rnew) => mtree match {
              case Tree2(llow, rlow, lowsep) => Tree2(ltree, Tree3(llow, rlow, rnew, lowsep, rsep), lsep).left
              case Tree3(llow, mlow, rlow, rlsep, rrsep) => Tree3(ltree, Tree2(llow, mlow, rlsep), Tree2(rlow, rnew, rsep), lsep, rrsep).left
            }
          }
        }
      }
    }
    }

}
