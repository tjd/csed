package basics;

public class ArithmeticWithDoubles {

	public static void main(String[] args) {
		double a = 3.22;
		double b = 6.0065;

		double sum = a + b;
		double product = a * b;
		double difference = a - b;
		double quotient = a / b;

		System.out.println("sum = " + sum);
		System.out.println("product = " + product);
		System.out.println("difference = " + difference);
		System.out.println("quotient = " + quotient);
		System.out.println("remainder = " + remainder);
		
		System.out.println();
		
		// controlling numbers after the decimal point
		System.out.printf("sum = %.2f\n", sum);
		System.out.printf("product= %.2f\n", product);
		System.out.printf("difference = %.2f\n", difference);
		System.out.printf("quotient = %.2f\n", quotient);
		System.out.printf("remainder = %.2f\n", remainder);
	}

}
