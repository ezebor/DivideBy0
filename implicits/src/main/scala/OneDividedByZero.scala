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

  /*
  1- Crear una sucesión de divisores candidatos que converja al divisor que necesitamos
  2- Crear una sucesión de cocientes, efectuando la división entre el dividendo y cada uno de los divisores candidatos
  3- Hallar la convergencia en la sucesión de cocientes
  4- Hallar el cociente target a partir del cual se considera convergente a la sucesión
   */

  case class Division(dividend: Double, divisor: Double) {
    /*def limit(number: Double): Double = Math.min(Math.max(number, Double.MinValue), Double.MaxValue)

    def foo = {
      lazy val infiniteResults: LazyList[(Double, Boolean)] = LazyList.iterate(((dividend + divisor) / 2, false)){
        case (currentDivisor, _) =>
          val newDivisor = (currentDivisor + divisor) / 2
          val currentResult = limit(dividend / currentDivisor)
          val newResult = limit(dividend / newDivisor)
          val newHasConverded = Math.abs(newResult - currentResult) <= 1E-10
          (newDivisor, newHasConverded)
      }

      val finalDivisor = infiniteResults
        .dropWhile{case (_, hasConverged) => !hasConverged}
        .head
        ._1

      limit(dividend / finalDivisor)
    }

    def divide() = {

    }*/

    def /~(number1: Double, number2: Double) = Math.min(Math.max(number1 / number2, Double.MinValue), Double.MaxValue)

    def divide() = {
      // step 1
      lazy val divisorsCandidates = LazyList.iterate(dividend)(divisorCandidate => (divisorCandidate + divisor) /~ 2)
      // step 2
      lazy val quotientsCandidates = divisorsCandidates.map(divisorCandidate => dividend /~ divisorCandidate)
      // step 3
      var quotientTarget = quotientsCandidates.head
      val convergence = quotientsCandidates.tail.map { currentQuotient =>
        val convergencePoint = (currentQuotient, Math.abs(quotientTarget - currentQuotient) <= 1E-10)
        quotientTarget = currentQuotient
        convergencePoint
      }
      // step 4
      convergence
        .dropWhile{case (_, hasConverged) => !hasConverged}
        .head
        ._1
    }

  }

  println(Division(1.0, 0.0).divide())
}
