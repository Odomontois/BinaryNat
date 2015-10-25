package com.awt.tree
/**
  * User: Oleg
  * Date: 25-Oct-15
  * Time: 14:47
  */
sealed trait Tree23[+A, +B]

case class Tree2[+A, +B](ltree: B, rtree: B, sep: A) extends Tree23[A, B]
case class Tree3[+A, +B](ltree: B, mtree: B, rtree: B, lsep: A, rsep: A) extends Tree23[A, B]


