package javaintro;

/*
 * 
 * Average.java
 * 
 * Reads in a series of number and returns their average.
 * 
 */

import java.util.Scanner;

public class Average {

	public static void main(String[] args) {
		Scanner in = new Scanner(System.in);
		System.out.print("How many numbers do you want to enter? ");
		int count = in.nextInt();

		double total = 0.0;
		for (int i = 0; i < count; ++i) {
			System.out.printf("Number %d --> ", i + 1);
			double num = in.nextDouble();
			total = total + num;
		}

		System.out.printf("\nAverage = %.2f", total / count);
	}

}
