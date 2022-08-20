import java.math.BigDecimal;
import java.math.RoundingMode;

public class JavaDivisionAlgorithm {
    public static class Division {
        private double dividend, divisor;

        public Division(double aDividend, double aDivisor) {
            dividend = aDividend;
            divisor = aDivisor;
        }

        private double customDivision(double number1, double number2) {
            return Math.min(Math.max(number1 / number2, Double.MIN_VALUE), Double.MAX_VALUE);
        }

        public double quotient() {
            double previousDivisor = customDivision(dividend + divisor, 2.0);
            double currentDivisor, previousResult, currentResult;
            do {
                currentDivisor = customDivision(previousDivisor + divisor, 2.0);

                previousResult = customDivision(dividend, previousDivisor);
                currentResult = customDivision(dividend, currentDivisor);

                previousDivisor = currentDivisor;
            } while (Math.abs(currentResult - previousResult) >= 1E-10);
            return currentResult;
        }
    }

    public static void main(String[] args) {
        Double result = new Division(1.0, 0.0).quotient();
        System.out.println(new BigDecimal(result).setScale(2, RoundingMode.HALF_UP).doubleValue());
    }
}
