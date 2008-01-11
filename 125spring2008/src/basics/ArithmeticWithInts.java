package basics;

public class ArithmeticWithInts {

	public static void main(String[] args) {
		int a = 22;
		int b = 5;

		int sum = a + b;
		int product = a * b;
		int difference = a - b;
		int quotient = a / b;
		int remainder = a % b;

		System.out.println("sum = " + sum);
		System.out.println("product = " + product);
		System.out.println("difference = " + difference);
		System.out.println("quotient = " + quotient);  // careful with this one!
		System.out.println("remainder = " + remainder);
		
		int c = -3;
		int calc1 = a + b * c;  // * done before +, as in regular arithmetic
		System.out.println("calc1 = " + calc1);
	}

}
