package com.dg.collection.immutable

import org.specs._
import org.scalacheck._

class LazyQueueSpecsTest extends org.specs.runner.JUnit4(LazyQueueSpecs)
object LazyQueueSpecs extends Specification with Scalacheck {
  import Prop._
  
  val emptyQ: LazyQueue[Int] = LazyQueue.Empty
  
  "lazy queue" should {
    
    "have length 0" in {
      emptyQ.length mustEqual 0
      emptyQ.isEmpty mustEqual true
    }
  
    "enqueue a single element at the end" in {
      val prop = property { i: Int =>
        val q = emptyQ.enqueue[Int](i)
        (q.length mustEqual 1) && (q.elements.toList(0) == i)
      }
      prop must pass
    }
  
    "enqueue 2 elements back to back at the end" in {
      val prop = property { (i: Int, j: Int) =>
        val q = emptyQ.enqueue[Int](i).enqueue[Int](j)
        (q.length mustEqual 2) && (q.elements.toList(0) == i) && (q.elements.toList(1) == j)
      }
      prop must pass
    }
    
    "enqueue multiple elements at the end of an empty queue" in {
      val LENGTH = 100
      val q = emptyQ.enqueue[Int]((0 until LENGTH).toList)
      q.length mustEqual LENGTH
      q.isEmpty mustEqual false
    }
    
    "enqueue a single element at the beginning" in {
      val prop = property { i: Int =>
        // enqueue 10 elements from back
        // last element is 10
        // so a next dequeue will get 0
        val q = emptyQ.enqueue[Int]((0 until 10).toList)
        q.length mustEqual 10
        
        // try a dequeue
        val (e, q1) = q.dequeue
        e mustEqual 0
        
        // enqueue at the front
        val q2 = q1.enqueuef[Int](99)
        
        // next dequeue should get 99
        val (f, q3) = q2.dequeue
        f mustEqual 99
        
        q3.length mustEqual 9
      }
      prop must pass
    }
    
    "dequeue from front" in {
      val prop = property { (i: Int, j: Int) =>
        val q = emptyQ.enqueue[Int](i).enqueue[Int](j)
        val (e, qr) = q.dequeue
        val (f, qq) = qr.dequeue
        (e mustEqual i) && (f mustEqual j)
      }
      prop must pass
    }
  }
}
