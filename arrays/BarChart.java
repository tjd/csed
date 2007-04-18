package arrays;

import java.util.Scanner;

public class BarChart {

	public static void main(String[] args) {
		// initialize the scanner and array
		Scanner sc = new Scanner(System.in);

		// create a new array that holds exactly 5 ints
		int[] arr = new int[5];

		// ask the user to enter the numbers
		for (int i = 0; i < arr.length; ++i) {
			System.out.printf("Please enter numnber %s: ", i + 1);
			int n = sc.nextInt();
			arr[i] = n;
		}

		// print a blank line for neatness
		System.out.println();

		for (int element : arr) {
			String stars = makeStars(element);
			System.out.println(stars);
		}
	}

	public static String makeStars(int n) {
		String result = "";
		for (int i = 0; i < n; ++i) {
			result = result + "*";
		}
		return result;
	}
}
