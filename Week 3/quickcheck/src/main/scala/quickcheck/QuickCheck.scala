package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  //If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("findMin") = forAll { (a: Int, b: Int) =>
    findMin(insert(b, insert(a, empty))) == scala.math.min(a, b)
  }
  //If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("deleteMin") = forAll { (a: Int) =>
    isEmpty(deleteMin(insert(a, empty)))
  }
  //Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("sorted") = forAll { a: H =>
    def isSorted(min: Int, h: H): Boolean =
      if (isEmpty(h))
        true
      else if(min > findMin(h))
        false
      else
        isSorted(findMin(h), deleteMin(h))

    isSorted(findMin(a), deleteMin(a))
  }
  //Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("meld") = forAll { (a: H, b: H) =>
    findMin(meld(a, b)) == scala.math.min(findMin(a), findMin(b))
  }


  property("delMin2") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(c, insert(b, insert(a, empty)))
    findMin(deleteMin(deleteMin(h))) == scala.math.max(c, scala.math.max(a, b))
  }



}
