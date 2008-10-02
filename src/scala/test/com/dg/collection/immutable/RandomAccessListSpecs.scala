package com.dg.collection.immutable

import org.specs._
import org.scalacheck._

class RandomAccessListSpecsTest extends org.specs.runner.JUnit4(RandomAccessListSpecs)
object RandomAccessListSpecs extends Specification with Scalacheck {
  import Prop._
  
  val emptyList: RandomAccessList[Int] = RandomAccessList.empty
  
  "random access list" should {
    
    "have length 0" in {
      emptyList.length mustEqual 0
    }
  
    "store a single element" in {
      val prop = property { i: Int =>
        val list = i :: emptyList
        (list.length mustEqual 1) && (list(0) == i)
      }
      prop must pass
    }
  
    "store multiple elements" in {
      val prop = property { (i: Int, j: Int) =>
        val list = j :: (i :: emptyList)
        (list.length mustEqual 2) && (list(0) == j)
      }
      prop must pass
    }
    
    "store lots of elements" in {
      val LENGTH = 1000
      val list = (0 until LENGTH).reverse.foldLeft(RandomAccessList.empty[Int])((x, y) => y :: x)
      list.length mustEqual LENGTH
      for (i <- 0 until LENGTH) {
        list(i) mustEqual i
      }
    }
  }
}
