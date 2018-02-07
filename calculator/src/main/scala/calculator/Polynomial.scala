package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    Signal((b() * b()) - (4 * a() * c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]):
  Signal[Set[Double]] = {
    if (delta() < 0)
      Signal(Set())
    else {
      val sqrt = Math.sqrt(delta())
      val twiceA = 2 * a()
      val minusB = -b()
      Signal(Set((minusB + sqrt) / twiceA, (minusB - sqrt) / twiceA))
    }
  }
}
