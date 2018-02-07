package calculator

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {

  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {

    def parseExpr(expr: Expr): List[String] = expr match {
      case Literal(v) => List(v.toString)
      case Ref(n) => List(n)
      case Plus(a, b) => parseExpr(a) ++: parseExpr(b)
      case Minus(a, b) => parseExpr(a) ++: parseExpr(b)
      case Times(a, b) => parseExpr(a) ++: parseExpr(b)
      case Divide(a, b) => parseExpr(a) ++: parseExpr(b)
      case _ => List()
    }

    val dependencyMap: Map[String, List[String]] = namedExpressions.map { case (key, value) => (key, parseExpr(value())) }

    def getValue(key: String) = {
      val expr = namedExpressions(key)
      if (check(key, dependencyMap(key), dependencyMap, false)) {
        Signal(Double.NaN)
      } else {
        Signal(eval(expr(), namedExpressions))
      }
    }

    def check(name: String, keys: List[String], map: Map[String, List[String]], result: Boolean ): Boolean = {
      if(keys.isEmpty || result) {
        result
      } else {
        check(name, keys flatMap { key =>  map get key} flatten, map, keys.contains(name) || result)
      }
    }

    namedExpressions.foldLeft(Map.empty[String, Signal[Double]]) {
      case (map, (key, value)) => map + (key -> getValue(key))
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
    case Literal(v) => v
    case Ref(name) => eval(getReferenceExpr(name, references), references)
    case Plus(a, b) => eval(a, references) + eval(b, references)
    case Minus(a, b) => eval(a, references) - eval(b, references)
    case Times(a, b) => eval(a, references) * eval(b, references)
    case Divide(a, b) => eval(a, references) / eval(b, references)
    case _ => Double.NaN
  }

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } {
      exprSignal => exprSignal()
    }
  }
}
