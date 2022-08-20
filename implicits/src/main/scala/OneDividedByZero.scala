import scala.math.BigDecimal.RoundingMode

object OneDividedByZero extends App {
  case class Division(dividend: Double, divisor: Double) {
    implicit class CustomDivision(number1: Double) {
      def /~(number2: Double): Double = Math.min(Math.max(number1 / number2, Double.MinValue), Double.MaxValue)
    }

    def quotient = {
      // step 1: create succession of infinite divisors
      lazy val divisorsCandidates = LazyList.iterate(dividend)(divisorCandidate => (divisorCandidate + divisor) /~ 2)
      // step 2: calculate infinite quotients
      lazy val quotientsCandidates = divisorsCandidates.map(divisorCandidate => dividend /~ divisorCandidate).zipWithIndex
      // step 3: return the quotient to which the succession converges
      quotientsCandidates
        .tail
        .dropWhile{case (quotient, index) => Math.abs(quotient - quotientsCandidates(index - 1)._1) >= 1E-10}
        .head
        ._1
    }
  }

  val result = Division(1.0, 0.0).quotient
  println(BigDecimal(result).setScale(2, RoundingMode.HALF_UP).toDouble)
}
