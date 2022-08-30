import scala.math.BigDecimal.RoundingMode

object ScalaDivisionAlgorithm extends App {
  implicit class CustomDivision(number1: Double) {
    def /~(number2: Double): Double = Math.min(Math.max(number1 / number2, Double.MinValue), Double.MaxValue)
  }

  def divide(number1: Double, number2: Double) = Math.min(Math.max(number1 / number2, Double.MinValue), Double.MaxValue)

  def quotient(dividend: Double, originalDivisor: Double): Double = {
    // step 1: create succession of infinite divisors
    val divisors = LazyList.iterate(dividend)(aDivisor => divide(aDivisor + originalDivisor, 2))

    // step 2: calculate infinite quotients
    val quotients = divisors.map(aCandidate => divide(dividend, aCandidate)).zipWithIndex

    // step 3: return the quotient to which the succession converges
    quotients
      .tail
      .dropWhile{case (quotient, index) => Math.abs(quotient - quotients(index - 1)._1) >= 1E-10}
      .head
      ._1
  }

  val tenDividedByTwo = quotient(10.0, 2.0)
  println(tenDividedByTwo)
  println(BigDecimal(tenDividedByTwo).setScale(2, RoundingMode.HALF_UP).toDouble)

  val threeDividedByTwo = quotient(3.0, 2.0)
  println(threeDividedByTwo)
  println(BigDecimal(threeDividedByTwo).setScale(2, RoundingMode.HALF_UP).toDouble)

  val oneDividedByZero = quotient(1.0, 0.0)
  println(oneDividedByZero)
  println(BigDecimal(oneDividedByZero).setScale(2, RoundingMode.HALF_UP).toDouble)
}
