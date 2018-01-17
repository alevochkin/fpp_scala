package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = //Gen.const(empty)
    oneOf(Gen.const(empty),
      for {
        elem <- arbitrary[Int]
        heap <- genHeap
      } yield insert(elem, heap))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    val h1 = insert(b, h)
    findMin(h1) == List(a, b).min
  }

  property("min3") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("min4") = forAll { (h: H) =>
    def iterate(h: H, acc: List[Int]): List[Int] = {
      if (isEmpty(h)) acc
      else {
        val min = findMin(h)
        iterate(deleteMin(h), min :: acc)
      }
    }

    def isSorted(o: List[Int]): Boolean =
      if (o.lengthCompare(1) > 0) {
        val ordering = scala.math.Ordering.Int
        o.seq.sliding(2).forall { duo => ordering.lteq(duo(0), duo(1)) }
      }
      else
        true

    isSorted(iterate(h, List()))
  }

  property("min5") = forAll { (h1: H, h2: H) =>
    val min1 = if (isEmpty(h1)) 0 else findMin(h1)
    val min2 = if (isEmpty(h2)) 0 else findMin(h2)
    if (isEmpty(h1) && isEmpty(h2))  0 == List(min1, min2).min else findMin(meld(h1, h2)) == List(min1, min2).min
  }

}
