package com.dg.collection.immutable

private [immutable] trait Tree[+T]
private [immutable] case class Node[+T](data: T, left: Tree[T], right: Tree[T]) extends Tree[T]
private [immutable] case class Leaf[+T](data: T) extends Tree[T]
private [immutable] case object E extends Tree[Nothing]

private [immutable] object Tree {
  def lookup[T](size: Int, t: Tree[T], i: Int): T = t match {
    case Leaf(x) => i match {
      case 0 => x
      case _ => throw new ArrayIndexOutOfBoundsException
    }
    case Node(x, l, r) => i match {
      case 0 => x
      case _ =>
        val s: Int = size / 2
        if (i <= s) {
          lookup(s, l, i - 1)
        } else {
          lookup(s, r, (i - 1 - s))
        }
    }
  }
  
  def update[T](size: Int, t: Tree[T], i: Int, y: T): Tree[T] = t match {
    case Leaf(x) => i match {
      case 0 => Leaf(y)
      case _ => throw new ArrayIndexOutOfBoundsException
    }
    case Node(x, l, r) => i match {
      case 0 => Node(y, l, r)
      case _ =>
        val s: Int = size / 2
        if (i <= s) {
          Node(x, update(s, l, i - 1, y), r)
        } else {
          Node(x, l, update(s, r, (i - 1 - s), y))
        }
    }
  }
}