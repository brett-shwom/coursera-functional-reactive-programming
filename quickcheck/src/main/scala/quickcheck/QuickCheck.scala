package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import scala.math

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { a: Int =>
  	var h = insert(a,empty)
  	var b = a+1
  	var min = math.min(b,a)
  	h = insert(b,h)
  	findMin(h) == min
  }

  property("delete") = forAll { a : Int =>
  	val h = insert(a, empty)
  	deleteMin(h) == empty
  }

  property("sorted") = forAll { h : H =>
  	var sortedHeap = sort(h)
  	sortedHeap == sortedHeap.sorted
  }

  property("minMeld") = forAll { (h1 : H, h2 : H) =>
  	println(h1)
  	println(h2)
  	println()
  	var min = findMin(meld(h1,h2))
  	min == findMin(h1) || findMin(h2) == min

  }

  def sort(h : H) : List[Int] = { 
  	sort(h,List())
  }

  def sort(h : H, sorted : List[Int]) : List[Int] = {
  	if (isEmpty(h)) sorted
  	else sort(deleteMin(h), sorted ::: List(findMin(h)))
  }

  // lazy val genHeap: Gen[H] = {
  // 	var h = empty
  // 	h = insert(1,h)
  // 	h = insert(-198,h)
  // 	h = insert(123,h)
  // 	h = insert(1098789980,h)
  // 	h = insert(54,h)
  // 	h
  // }


  lazy val genHeap: Gen[H] = for {    
    v <- arbitrary[A]    
    h <- frequency((1, value(empty)), (3, genHeap))  
  } yield if (isEmpty(h) && v == 0) h else insert(v, h)

  lazy val genHeapNonEmpty: Gen[H] = genHeap suchThat(!isEmpty(_))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeapNonEmpty)

}
