package javaintro;

import java.util.Scanner;

/*
 * 
 * YesOrNo.java
 * 
 * Demonstrates use of char data for getting a yes/no repsonse from the user.
 * 
 */

public class YesOrNo {

	public static void main(String[] args) {
		Scanner in = new Scanner(System.in);

		System.out.print("Are you 18 or over? (y/n) ");

		String ans = in.next();
		char first = ans.charAt(0);

		if (first == 'y') {
			System.out.println("\nYou may view the pictures that follow ...");
		} else {
			System.out
					.println("\nSorry, you are too immature to witness what follows. Go away.");
		}

	}

}
