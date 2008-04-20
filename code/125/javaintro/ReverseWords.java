package javaintro;

/*
 * 
 * ReverseWords.java
 * 
 * Reads in a sentence, and prints out the sentence with the indiviudual words reversed.
 * For example:
 * 
 * Input: this is a test
 * Output: siht si a tset
 * 
 */

import java.util.Scanner;

public class ReverseWords {

	// Returns a new string that is the reverse of s.
	public static String reverse(String s) {
		String result = "";
		for (int i = 0; i < s.length(); ++i) {
			result = s.charAt(i) + result;
		}
		return result;
	}

	public static void main(String[] args) {
		Scanner in = new Scanner(System.in);

		System.out.print("Enter a sentence: ");
		while (in.hasNext()) {
			String word = in.next();
			String rev = reverse(word);
			System.out.print(rev + " ");
		}
		System.out.println("done");
	}

}
