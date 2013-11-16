package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll {
    a: Int =>
      val h = insert(a, empty)
      findMin(h) == a
  }

  property("insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.") = forAll {
    (a: Int, b: Int) =>
      (a < b) ==> {
        val th = insert(a, empty)
        val h = insert(b, th)
        findMin(h) == a

      }
  }

  property("insert an element into an empty heap, then delete the minimum, the resulting heap should be empty") = forAll {
    a: Int => {
      val t = insert(a, empty)
      val h = deleteMin(t)
      isEmpty(h)

    }
  }
  property("you should get a sorted sequence of elements when continually finding and deleting minima") = forAll {
    h: H => {

      def checkMin(acc: H, min: Int): (H, Int, Boolean) = {
        if (isEmpty(acc)) (empty, min, true)
        else {
          val min2 = findMin(acc)
          if (min2 < min) (empty, min, false)
          else {
            checkMin(deleteMin(acc), min2)
          }
        }
      }
      val (_, _, ok) = checkMin(h, Int.MinValue)
      ok
    }
  }


  property("a minimum of the melding of any two heaps should return a minimum of one or the other") = forAll {
    (h1: H, h2: H) => (h1 != h2) ==> {
      val h = meld(h1, h2)
      val min = findMin(h)
      val min1 = findMin(h1)
      val min2 = findMin(h2)
      val v = Math.min(min1, min2)
      min == v
    }
  }


  property("node count of the result melt should be the same as sum of the two originals") = forAll {
    (h1: H, h2: H) => (h1 != h2) ==> {
      def nodeCount(acc: H, flat: Set[Int]): (H, Set[Int]) = {
        if (isEmpty(acc)) (empty, flat)
        else {
          val min = findMin(acc)
          nodeCount(deleteMin(acc), flat + min)
        }
      }

      val h = meld(h1, h2)
      val s = nodeCount(h, Set.empty)
      val s1 = nodeCount(h1, Set.empty)
      val s2 = nodeCount(h2, Set.empty)
      s._2 == (s1._2 ++ s2._2)
    }
  }
  /* */

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    m <- oneOf(value(empty), genHeap)
  } yield insert(i, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


}
