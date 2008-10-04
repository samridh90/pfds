package com.dg.collection.immutable

/**
 * Implementation of a functional queue with O(log n) worst case and O(1) amortized performance. The implementation
 * uses lazy list implementation of Scala (Stream) and incremental pre-evaluation of tail.
 * <p/>
 * The implementation is based on Okasaki's paper 
 * <a href="http://www.eecs.usma.edu/webs/people/okasaki/pubs.html#jfp95">Simple and Efficient Purely Functional Queues and Deques</a>.
 */
object LazyQueue {
  val Empty: LazyQueue[Nothing] = 
    new LazyQueue(Stream.empty, 0, Nil, 0)
}

class LazyQueue[+A] private (front: Stream[A], sizef: Int, rear: List[A], sizer: Int) {

  def this() {
    this(Stream.empty, 0, Nil, 0)
  }
  
  /**
   * A private function that does an <i>incremental</i> reversal of <code>rear</code> to prevent the long pauses caused
   * in standard <code>Queue</code> implementation of Scala library. In the standard Scala implementation, when <code>front</code>
   * becomes empty, <code>rear</code> needs to be reversed and then copied to <code>front</code>, leading to a potentially
   * O(n) operation. Instead <code>rotate</code> incrementally replaces <front, rear> by <front ++ reverse(rear)>.
   * <p/>
   * Invariants:
   * <li><code>sizef == front.length && sizer == rear.length</code></li>
   * <li><code>sizef >= sizer</code></li>
   * <p/>
   * <code>rotate</code> is called when <code>ys.length == xs.length + 1</code>
   * <p/>
   * <em>See Okasaki's paper for details of the analysis.</em>
   * 
   * @param xs the front part of the queue
   * @param ys the rear part of the queue
   * @param rys the incremental reverse of ys
   * 
   * @return the cons'd Stream after rotation
   */
  private [this] def rotate[A](xs: Stream[A], ys: List[A], rys: Stream[A]): Stream[A] = ys match {
    case y :: ys1 =>
      if (xs isEmpty) Stream.cons(y, rys)
      else
        Stream.cons(xs.head, rotate(xs.tail, ys.tail, Stream.cons(y, rys)))
    case Nil =>
      throw new NullPointerException("ys is null")
  }

  /**
   * Factory method that constructs a queue.
   * 
   * @param f the front
   * @param sf size of front
   * @param r the rear part of the queue in reversed order
   * @param sr size of rear
   * 
   * @return an instance of <code>LazyQueue</code>
   */
  protected [immutable] def makeQ[A](f: Stream[A], sf: Int, r: List[A], sr: Int): LazyQueue[A] = {
    if (sr <= sf) {
      new LazyQueue[A](f, sf, r, sr)
    } else {
      // rotate is called when sr == sf + 1
      new LazyQueue[A](rotate(f, r, Stream.empty), sf + sr, Nil, 0)
    }
  }
  
  /**
   * Checks if the queue is empty.
   * sizef == 0 => sizer == 0 (by invariant above)
   */
  def isEmpty: Boolean = sizef == 0
  
  def enqueue[B >: A](elem: B) = {
    makeQ(front, sizef, elem :: rear, sizer + 1)
  }
  
  def enqueue[B >: A](elems: Iterable[B]): LazyQueue[B] = {
    (elems :\ (this: LazyQueue[B]))((x, y) => y.enqueue(x)) 
  }
  
  def dequeue: (A, LazyQueue[A]) = {
    (front.head, makeQ(front.tail, sizef - 1, rear, sizer))
  }
  
  def elements: Iterator[A] = (front.force ::: rear.reverse).elements
  
  def length = (sizef + sizer)
}
