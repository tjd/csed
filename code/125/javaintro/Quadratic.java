package javaintro;

/*
 * 
 * Quadratic.java
 * 
 * Calculates the real roots of a quadratic equation. It demonstrates the following Java features:
 * 
 *   - Scanner input
 *   - variable declarations
 *   - arithmetic expressions
 *   - if/else statement
 *   - formatted output with the printf statement
 * 
 */

import java.util.Scanner;

public class Quadratic {

	public static void main(String[] args) {
		// create input scanning object
		Scanner in = new Scanner(System.in);

		// read in the coefficients
		System.out.print("Please enter coefficient a: ");
		double a = in.nextDouble();
		System.out.print("Please enter coefficient b: ");
		double b = in.nextDouble();
		System.out.print("Please enter coefficient c: ");
		double c = in.nextDouble();

		// calculate the discriminant
		double discr = b * b - 4 * a * c;
		if (discr < 0) {
			System.out.printf("No real solution for coefficients %f, %f, %f",
					a, b, c);
		} else {
			double sol1 = (-b + Math.sqrt(discr)) / 2 * a;
			double sol2 = (-b - Math.sqrt(discr)) / 2 * a;
			System.out.printf("\nsolution 1 = %.2f\nsolution 2 = %.2f", sol1,
					sol2);
		}
	}

}
