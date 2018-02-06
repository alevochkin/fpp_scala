package calculator

import calculator.TweetLength.MaxTweetLength
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, _}

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /** ****************
    * * TWEET LENGTH **
    * *****************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("Self dependency") {
    val sourceMap: Map[String, Signal[Expr]] = Map("a" -> Signal(Plus(Ref("a"), Ref("b"))))
    val result = Calculator.computeValues(sourceMap)
    assert(result("a")() equals Double.NaN)
  }

  test("Cyclic dependency") {
    val sourceMap: Map[String, Signal[Expr]] = Map("a" -> Signal(Plus(Ref("c"), Ref("b"))), "b" -> Signal(Literal(100)), "c" -> Signal(Plus(Ref("a"), Literal(100))))
    val result = Calculator.computeValues(sourceMap)
    assert(result("a")() equals Double.NaN)
    assert(result("c")() equals Double.NaN)
  }

  test("Cyclic dependency deep") {
    val sourceMap: Map[String, Signal[Expr]] = Map("a" -> Signal(Plus(Ref("с"), Ref("b"))),
      "b" -> Signal(Plus(Ref("d"), Ref("e"))),
      "c" -> Signal(Literal(50)),
      "d" -> Signal(Literal(50)),
      "e" -> Signal(Ref("a"))
    )
    val result = Calculator.computeValues(sourceMap)
    assert(result("a")() equals Double.NaN)
  }

  test("Unknown key") {
    val sourceMap: Map[String, Signal[Expr]] = Map("a" -> Signal(Plus(Literal(100), Ref("b"))), "b" -> Signal(Literal(100)), "z" -> Signal(Plus(Ref("s"), Literal(100))))
    val result = Calculator.computeValues(sourceMap)
    assert(result("a")() == 200.0)
    assert(result("z")() equals Double.NaN)
  }

}
