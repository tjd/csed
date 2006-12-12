package javaintro;

import java.util.Random;
import java.util.Scanner;

/*
 * Dice.java
 * 
 * Simulates rolling dice.
 * 
 */

public class Dice {

	// rnd and in are available to any method in the Dice class
	private static Random rnd = new Random();

	private static Scanner in = new Scanner(System.in);

	// returns a random int from 1 .. sides
	public static int roll(int sides) {
		return 1 + rnd.nextInt(sides);
	}

	public static void main(String[] args) {
		System.out.print("How many sides does your die have? ");
		int sides = in.nextInt();
		System.out.print("How many times should I roll it? ");
		int rolls = in.nextInt();

		int sum = 0;
		for (int i = 0; i < rolls; ++i) {
			sum += roll(sides);
		}

		System.out.println("Sum = " + sum);
		System.out.println("Average = " + (sum / (double) rolls));
	}

}
