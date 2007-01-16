package passgen;

import java.util.Random;

public class RandomPassword {

	private static Random rnd = new Random();

	private static final String letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

	public static char oneRandomLetter() {
		int r = rnd.nextInt(letters.length());
		return letters.charAt(r);
	}

	public static String genPassword() {
		String result = "";
		int numLetters = 8 + rnd.nextInt(3);
		for(int i = 0; i < numLetters; ++i) {
			result += oneRandomLetter();
		}
		return result;
	}
	
	public static void main(String[] args) {
		System.out.println(genPassword());
		System.out.println(genPassword());
		System.out.println(genPassword());
	}

}
