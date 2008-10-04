package com.dg.collection.immutable

/**
 * Implementation of a functional queue with O(1) worst case performance. The implementation
 * uses lazy list implementation of Scala (Stream) and incremental pre-evaluation of tail. 
 * In {@link LazyQueue}, evaluation of tails result in O(log n) worst case performance.
 * In the current implementation we use stronger memoization which guarantees every tail
 * of the form <code>rotate(xs.tail, ..</code> has been preevaluated and hence gives O(1)
 * performance.
 * <p/>
 * The implementation is based on Okasaki's paper 
 * <a href="http://www.eecs.usma.edu/webs/people/okasaki/pubs.html#jfp95">Simple and Efficient Purely Functional Queues and Deques</a>.
 */
object MemoizedQueue {
  val Empty: MemoizedQueue[Nothing] = 
    new MemoizedQueue(Stream.empty, Nil, Stream.empty)
}

class MemoizedQueue[+A] private (front: Stream[A], rear: List[A], pending: Stream[A]) {

  def this() {
    this(Stream.empty, Nil, Stream.empty)
  }
  
  /**
   * A private function that does an <i>incremental</i> reversal of <code>rear</code> to prevent the long pauses caused
   * in standard <code>Queue</code> implementation of Scala library. In the standard Scala implementation, when <code>front</code>
   * becomes empty, <code>rear</code> needs to be reversed and then copied to <code>front</code>, leading to a potentially
   * O(n) operation. Instead <code>rotate</code> incrementally replaces <front, rear> by <front ++ reverse(rear)>.
   * <p/>
   * Invariants:
   * <li><code>front.length >= rear.length</code></li>
   * <li><code>pending.length = front.length - rear.length</code></li>
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
   * @param r the rear part of the queue in reversed order
   * @param p some tail of <code>f</code> marking the boundary between evaluated and 
   *          unevaluated portions of <code>f</code>
   * 
   * @return an instance of <code>MemoizedQueue</code>
   */
  protected [immutable] def makeQ[A](f: Stream[A], r: List[A], p: Stream[A]): MemoizedQueue[A] = {
    if (p isEmpty) {
      val fr = rotate(f, r, Stream.empty)
      new MemoizedQueue[A](fr, Nil, fr)
    } else {
      new MemoizedQueue[A](f, r, p.tail)
    }
  }
  
  def isEmpty: Boolean = front isEmpty
  
  def enqueue[B >: A](elem: B) = {
    makeQ(front, elem :: rear, pending)
  }
  
  def enqueue[B >: A](elems: Iterable[B]): MemoizedQueue[B] = {
    (elems :\ (this: MemoizedQueue[B]))((x, y) => y.enqueue(x)) 
  }
  
  def dequeue: (A, MemoizedQueue[A]) = {
    if (front isEmpty) throw new Exception("empty queue")
    (front.head, makeQ(front.tail, rear, pending))
  }
  
  def elements: Iterator[A] = (front.force ::: rear.reverse).elements
  
  def length = (pending.length + 2 * rear.length)
}
