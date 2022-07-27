object OneDividedByZero extends App {

  case class Operand(number: Double) {

    def /~(divisor: Operand): Double = {
      def betweenLimits(number: Double): Double = Math.min(Math.max(number, Double.MinValue), Double.MaxValue)

      def infiniteList(auxDivisor: Double): LazyList[(Double, Double)] = {
        val average = (auxDivisor + divisor.number) / 2
        (betweenLimits(number / auxDivisor), betweenLimits(number / average)) #:: infiniteList(average)
      }

      def converge(results: LazyList[(Double, Double)]): Double = results
        .filter { case (result1, result2) => Math.abs(result2 - result1) <= 1E-10}
        .head
        ._2

      converge(infiniteList(number))
    }
  }

  implicit def toNumber(n: Double): Operand = Operand(n)

  println(1.0 /~ 0.0)
}
