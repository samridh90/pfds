package com.dg.collection.immutable

/**
 * Implementation of a functional queue with O(1) worst case performance. 
 * The implementation uses lazy list implementation of Scala (Stream) and 
 * incremental pre-evaluation of tail. 
 * <p/>
 * In {@link LazyQueue}, evaluation of tails result in O(log n) worst case 
 * performance. In the current implementation we use stronger memoization 
 * which guarantees every tail of the form <code>rotate(xs.tail, ..</code> 
 * has been preevaluated and hence gives O(1) performance.
 * <p/>
 * The implementation is based on Okasaki's paper 
 * <a href="http://www.eecs.usma.edu/webs/people/okasaki/pubs.html#jfp95">Simple and Efficient Purely Functional Queues and Deques</a>.
 */
object MemoizedQueue {
  val Empty: MemoizedQueue[Nothing] = 
    new MemoizedQueue(Stream.empty, Nil, Nil)
}

class MemoizedQueue[+A] private (front: Stream[A], rear: List[A], pending: List[A]) {

  def this() {
    this(Stream.empty, Nil, Nil)
  }
  
  /**
   * A private function that does an <i>incremental</i> reversal of <code>rear</code> 
   * to prevent the long pauses caused in standard <code>Queue</code> implementation 
   * of Scala library. In the standard Scala implementation, when <code>front</code>
   * becomes empty, <code>rear</code> needs to be reversed and then copied to <code>front</code>, 
   * leading to a potentially O(n) operation. Instead <code>rotate</code> incrementally 
   * replaces <front, rear> by <front ++ reverse(rear)>.
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
   */
  private [this] def rotate[A](xs: Stream[A], ys: List[A], rys: List[A]): Stream[A] = ys match {
    case y :: ys1 =>
      if (xs isEmpty) (y :: rys).toStream  
      else
        Stream.cons(xs.head, rotate(xs.tail, ys1, y :: rys))
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
  protected [immutable] def makeQ[A](f: Stream[A], r: List[A], p: List[A]): MemoizedQueue[A] = {
    if (p isEmpty) {
      val fr = rotate(f, r, Nil)
      new MemoizedQueue[A](fr, Nil, fr.force)
    } else {
      new MemoizedQueue[A](f, r, p.tail)
    }
  }
  
  def isEmpty: Boolean = front isEmpty
  
  /** 
   *  Creates a new queue with element added at the end 
   *  of the old queue.
   *
   *  @param elem the element to insert
   */
  def enqueue[B >: A](elem: B) = {
    makeQ(front, elem :: rear, pending)
  }
  
  /** 
   *  Returns a new queue with all all elements provided by 
   *  an <code>Iterable</code> object added at the end of 
   *  the queue. 
   *  The elements are prepended in the order they
   *  are given out by the iterator.
   *
   *  @param iter an iterable object
   */
  def enqueue[B >: A](elems: Iterable[B]): MemoizedQueue[B] = {
    var q: MemoizedQueue[B] = MemoizedQueue.Empty
    elems.foreach(x => q = q.enqueue[B](x)) 
    q
  }
  
  /** 
   *  Returns a new queue with all elements added.
   *
   *  @param elems the elements to add.
   */
  def enqueue[B >: A](elems: B*): MemoizedQueue[B] = {
    var q: MemoizedQueue[B] = MemoizedQueue.Empty
    elems.foreach(x => q = q.enqueue[B](x)) 
    q
  }
  
  /** 
   *  Creates a new queue with element added at the front 
   *  of the old queue.
   *
   *  @param elem the element to insert
   */
  def enqueuef[B >: A](elem: B) = {
    makeQ(Stream.cons(elem, front), rear, elem :: pending)
  }
  
  /** 
   *  Returns a tuple with the first element in the queue,
   *  and a new queue with this element removed.
   *
   *  @throws Predef.NoSuchElementException
   *  @return the first element of the queue and a new queue.
   */
  def dequeue: (A, MemoizedQueue[A]) = {
    if (front isEmpty) throw new NoSuchElementException("queue empty")
    (front.head, makeQ(front.tail, rear, pending))
  }
  
  def elements: Iterator[A] = (front.force ::: rear.reverse).elements
  
  def length = (front.length + rear.length)
}
