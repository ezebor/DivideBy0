import scala.math.BigDecimal.RoundingMode

object OneDividedByZero extends App {

  case class Operand(number: Double) {

    def /~(divisor: Operand): Double = {
      def limit(number: Double): Double = Math.min(Math.max(number, Double.MinValue), Double.MaxValue)

      def infiniteDivisors(previousDivisor: Double): LazyList[(Double, Double)] = {
        val newDivisor = (previousDivisor + divisor.number) / 2
        (previousDivisor, newDivisor) #:: infiniteDivisors(newDivisor)
      }

      def converge(): Double =
        infiniteDivisors(divisor.number)
        .map {
          case (previousDivisor, nextDivisor) => (limit(number / previousDivisor), limit(number / nextDivisor))
        }
        .filter {
          case (previousResult, nextResult) => Math.abs(nextResult - previousResult) <= 1E-10
        }
        .head
        ._2

      converge()
    }
  }

  implicit def toNumber(n: Double): Operand = Operand(n)

  //println(1.0 /~ 0.0)

  case class Division(dividend: Double, divisor: Double) {
    def /~(number1: Double, number2: Double) = Math.min(Math.max(number1 / number2, Double.MinValue), Double.MaxValue)

    def divide() = {
      // step 1:
      lazy val divisorsCandidates = LazyList.iterate(dividend)(divisorCandidate => (divisorCandidate + divisor) /~ 2)
      lazy val quotientsCandidates = divisorsCandidates.map(divisorCandidate => dividend /~ divisorCandidate).zipWithIndex
      quotientsCandidates
        .tail
        .dropWhile{case (quotient, index) => Math.abs(quotient - quotientsCandidates(index - 1)._1) >= 1E-10}
        .head
        ._1
    }
  }

  val result = Division(3.0, 0.0).divide()
  println(BigDecimal(result).setScale(2, RoundingMode.HALF_UP).toDouble)
}
