package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  /*lazy val genHeap: Gen[H] = oneOf(Gen.const(empty),
      for {
        elem <- arbitrary[Int]
        heap <- genHeap
      } yield insert(elem, heap))*/

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency((1, Gen.const(empty)), (9, genHeap))
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("hint1") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    val h1 = insert(b, h)
    findMin(h1) == List(a, b).min
  }

  property("hint2") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("hint3") = forAll { (h: H) =>
    def isSorted(h: H): Boolean =
      if (isEmpty(h)) true
      else {
        val newMin = findMin(h)
        val newH = deleteMin(h)
        (isEmpty(newH) || newMin <= findMin(newH)) && isSorted(newH)
      }

    isSorted(h)
  }

  property("hint4") = forAll { (h1: H, h2: H) =>
    val min1 = if (isEmpty(h1)) 0 else findMin(h1)
    val min2 = if (isEmpty(h2)) 0 else findMin(h2)
    if (isEmpty(h1) && isEmpty(h2)) 0 == List(min1, min2).min else findMin(meld(h1, h2)) == List(min1, min2).min
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    heapEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

}
