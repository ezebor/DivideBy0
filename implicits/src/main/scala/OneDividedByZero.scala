object OneDividedByZero extends App {

  case class Operand(number: Double) {

    def /~(divisor: Operand): Double = {
      def infiniteList(auxDivisor: Double): LazyList[Double] = {
        val average = (auxDivisor + divisor.number) / 2
        val newResult = Math.min(Math.max(number / average, Double.MinValue), Double.MaxValue)
        newResult #:: infiniteList(average)
      }

      def converge(results: LazyList[Double]): Double = results match {
        case result1 #:: result2 #:: othersResults =>
          if(Math.abs(result2 - result1) <= 1E-10) result2
          else converge(result2 #:: othersResults)
      }

      converge(infiniteList(number))
    }
  }

  implicit def toNumber(n: Double): Operand = Operand(n)
  println(1.0 /~ 2.0)
}
