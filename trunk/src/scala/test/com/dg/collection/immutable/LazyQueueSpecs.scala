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
    }
  
    "enqueue a single element" in {
      val prop = property { i: Int =>
        val q = emptyQ enqueue(i)
        (q.length mustEqual 1) && (q.elements.toList(0) == i)
      }
      prop must pass
    }
  
    "enqueue multiple elements at the back" in {
      val prop = property { (i: Int, j: Int) =>
        val q = emptyQ.enqueue(i).enqueue(j)
        (q.length mustEqual 2) && (q.elements.toList(0) == i) && (q.elements.toList(1) == j)
      }
      prop must pass
    }
    
    "dequeue from front" in {
      val prop = property { (i: Int, j: Int) =>
        val q = emptyQ.enqueue(i).enqueue(j)
        val (e, qr) = q.dequeue
        val (f, qq) = qr.dequeue
        (e mustEqual i) && (f mustEqual j)
      }
      prop must pass
    }
  }
}
