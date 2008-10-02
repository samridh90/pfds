package com.dg.collection.immutable

/**
 * Implementation of a functional list data structure that offers O(log n) random access, in addition to
 * O(1) list primitive operations (head, tail, cons). The implementation is based on Okasaki's paper 
 * <a href="http://www.eecs.usma.edu/webs/people/okasaki/pubs.html#fpca95">Purely Functional Random-Access Lists</a>.
 */
object RAList {
  def empty[T]: RAList[T] = RAList.Nil
  
  private [immutable] case object Nil extends RAList[Nothing] {
    override def equals(that : Any) = this eq that.asInstanceOf[AnyRef] 
  }
  
  private [immutable] case class Root[+T](trees: List[(Int, Tree[T])])
    extends RAList[T] {
  }
  
}

import RAList._

sealed abstract class RAList[+T] {
  
  /** 
   *  Returns the ith element of the list
   *  
   *  @return element at ith index
   *  @throws Predef.NoSuchElementException if the list is empty.
   */
  def apply(i: Int) = {
    def lookup[T](ts: List[(Int, Tree[T])], i: Int): T = ts match {
      case scala.Nil => throw new NoSuchElementException
      case ((size, t) :: rest) =>
        if (i < size) {
          Tree.lookup(size, t, i)
        } else {
          lookup(rest, i - size)
        }
    }
    this match {
      case Nil => throw new NoSuchElementException
      case Root(trees) =>
        lookup(trees, i)
      case _ => throw new IllegalStateException
    }
  }
  
  /** 
   *  Returns true if the list does not contain any elements.
   *  @return <code>true</code>, iff the list is empty.
   */
  def isEmpty = this == Nil
  
  /** 
   *  Returns this first element of the list.
   *
   *  @return the first element of this list.
   *  @throws Predef.NoSuchElementException if the list is empty.
   */
  def head: T = this match {
    case Nil => throw new NoSuchElementException("head on empty list")
    case Root((size, Leaf(x)) :: rest) => x
    case Root((size, Node(x, t1, t2)) :: rest) => x
    case _ => throw new IllegalStateException
  }
  
  /** 
   *  Returns this list without its first element.
   *
   *  @return this list without its first element.
   *  @throws Predef.NoSuchElementException if the list is empty.
   */
  def tail: RAList[T] = this match {
    case Nil => throw new NoSuchElementException("tail on empty list")
    case Root((size, Leaf(x)) :: rest) => rest match {
        case scala.Nil => Nil
        case _ => Root(rest)
      }
    case Root((size, Node(x, t1, t2)) :: rest) =>
      val s = size / 2
      Root((s, t1) :: (s, t2) :: rest)
    case _ => throw new IllegalStateException
  }
  
  /** 
   *  Add an element <code>x</code> at the beginning of this list.
   *
   *  @param x the element to prepend.
   *  @return  the list with <code>x</code> added at the beginning.
   *  @ex <code>1 :: RAList(2, 3) = RAList(2, 3).::(1) = RAList(1, 2, 3)</code>
   */
  def ::[U >: T](x: U): RAList[U] = this match {
    case Root(xs @ ((size1, t1) :: (size2, t2) :: rest)) =>
      if (size1 == size2) {
        Root((1 + size1 + size2, Node(x, t1, t2)) :: rest)
      } else {
        Root((1, Leaf(x)) :: xs)
      }
    case Root(xs) =>
      Root((1, Leaf(x)) :: xs)
    case Nil =>
      Root(List((1, Leaf(x))))
    case _ => throw new IllegalStateException
  }
  
  /** 
   *  Returns a new list with the <code>i</code>th element updated to <code>value</code>
   *  
   *  @param i the position of element to be updated
   *  @param value the new value
   *  @return a new list with the <code>i</code>th element updated to <code>value</code>
   */
  def update[U >: T](i: Int, value: U): RAList[U] = {
    def update_a(ts: List[(Int, Tree[T])], i: Int, y: U): List[(Int, Tree[U])] = ts match {
      case scala.Nil => throw new NoSuchElementException("update on empty list")
      case ((size, t) :: rest) =>
        if (i < size) {
          (size, Tree.update(size, t, i, value)) :: rest
        } else {
          (size, t) :: update_a(rest, (i - size), y)
        }
    }
    this match {
      case Root(trees) =>
        Root(update_a(trees, i, value))
      case _ => throw new IllegalStateException
    }
  }
  
  def length = {
    var these = this
    var len = 0
    while (!these.isEmpty) {
      len += 1
      these = these.tail
    }
    len
  }
  
}
