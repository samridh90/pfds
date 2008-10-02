package com.dg.collection.immutable

/**
 * Implementation of a functional list data structure that offers O(log n) random access, in addition to
 * O(1) list primitive operations (head, tail, cons). The implementation is based on Okasaki's paper 
 * <a href="http://www.eecs.usma.edu/webs/people/okasaki/pubs.html#fpca95">Purely Functional Random-Access Lists</a>.
 */
object RandomAccessList {
  
  def empty[T]: RandomAccessList[T] = RandomAccessList.Nil
  
  private [immutable] case object Nil extends RandomAccessList[Nothing] {
    override def equals(that : Any) = this eq that.asInstanceOf[AnyRef] 
  }
  
  private [immutable] case class Root[+T](size: Int, tree: Tree[T], rest: RandomAccessList[T])
    extends RandomAccessList[T] {
      
  }
}

import RandomAccessList._

sealed abstract class RandomAccessList[+T] {
  /** 
   *  Returns this first element of the list.
   *
   *  @return the first element of this list.
   *  @throws Predef.NoSuchElementException if the list is empty.
   */
  def head: T = this match {
    case Nil => throw new NoSuchElementException("head on empty list")
    case Root(_, Leaf(x), _) => x
    case Root(_, Node(x, l, r), _) => x
    case _ => throw new IllegalStateException
  }
  
  /** 
   *  Returns this list without its first element.
   *
   *  @return this list without its first element.
   *  @throws Predef.NoSuchElementException if the list is empty.
   */
  def tail: RandomAccessList[T] = this match {
    case Nil => throw new NoSuchElementException("tail on empty list")
    case Root(_, Leaf(x), rest) => rest
    case Root(size, Node(x, l, r), rest) =>
      val s = size >> 2
      Root(s, l, Root(s, r, rest))
    case _ => throw new IllegalStateException
  }
  
  /** 
   *  Add an element <code>x</code> at the beginning of this list.
   *
   *  @param x the element to prepend.
   *  @return  the list with <code>x</code> added at the beginning.
   *  @ex <code>1 :: RAList(2, 3) = RAList(2, 3).::(1) = RAList(1, 2, 3)</code>
   */
  def ::[U >: T](x: U): RandomAccessList[U] = this match {
    case xs@Root(size1, l, Root(size2, r, rest)) =>
      if (size1 == size2) {
        Root(1 + size1 + size2, Node(x, l, r), rest)
      } else {
        Root(1, Leaf(x), xs)
      }
    case xs => Root(1, Leaf(x), xs)
  }
  
  /** 
   *  Returns true if the list does not contain any elements.
   *  @return <code>true</code>, iff the list is empty.
   */
  def isEmpty = this == Nil
  
  /** 
   *  Returns the ith element of the list
   *  
   *  @return element at ith index
   *  @throws Predef.NoSuchElementException if the list is empty.
   */
  def apply(i: Int): T = this match {
    case Nil => throw new NoSuchElementException
    case Root(size, t, rest) =>
      if (i < size) {
        Tree.lookup(size, t, i)
      } else {
        rest.apply(i - size)
      }
    case _ => throw new IllegalStateException
  }
  
  /** 
   *  Returns a new list with the <code>i</code>th element updated to <code>value</code>
   *  
   *  @param i the position of element to be updated
   *  @param value the new value
   *  @return a new list with the <code>i</code>th element updated to <code>value</code>
   */
  def update[U >: T](i: Int, value: U): RandomAccessList[U] = this match {
    case Nil => throw new NoSuchElementException
    case Root(size, t, rest) =>
      if (i < size) {
        Root(size, Tree.update(size, t, i, value), rest)
      } else {
        Root(size, t, rest.update(i - size, value))
      }
    case _ => throw new IllegalStateException
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
