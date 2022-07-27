object OneDividedByZero extends App {

  case class Number(dividend: Double) {

    def dividedBy(divisor: Double): Double = {

      def infiniteList(auxDivisor: Double): LazyList[Double] = {
        val average = (auxDivisor + divisor) / 2
        val newResult = Math.min(Math.max(dividend / average, Double.MinValue), Double.MaxValue)
        newResult #:: infiniteList(average)
      }

      def converge(results: LazyList[Double]): Double = results match {
        case result1 #:: result2 #:: othersResults =>
          if(Math.abs(result2 - result1) <= 1E-10) result2
          else converge(result2 #:: othersResults)
      }

      converge(infiniteList(dividend))
    }
  }
  implicit def toNumber(n: Double) = Number(n)
  println(3.0 dividedBy 0)
}
