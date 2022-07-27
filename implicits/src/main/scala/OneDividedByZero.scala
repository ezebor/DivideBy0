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

  println(1.0 /~ 0.0)
}
