package javaintro;

/**
 * 
 * Box.java
 * 
 * Reads in a string and prints it out in a box of *s.
 * 
 */

import java.util.Scanner;

public class Box {

	public static void main(String[] args) {
		// read in the input
		Scanner in = new Scanner(System.in);
		System.out.print("Enter text --> ");
		String text = in.nextLine();

		// create a string of *s
		int n = text.length();
		String stars = "****";
		for (int i = 0; i < n; ++i) {
			stars = stars + "*";
		}

		// print the text inside a box of *s
		System.out.println();
		System.out.println(stars);
		System.out.println("* " + text + " *");
		System.out.println(stars);

	}

}
