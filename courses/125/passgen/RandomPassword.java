package passgen;

import java.util.ArrayList;
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
		for (int i = 0; i < numLetters; ++i) {
			result += oneRandomLetter();
		}
		return result;
	}

	public static String genSyllablePassword() {
		ArrayList<String> syllables = new ArrayList<String>();
		syllables.add("bot");
		syllables.add("noo");
		syllables.add("burp");
		syllables.add("zimp");
		int numSyllables = 3 + rnd.nextInt(2);
		String result = "";
		for (int i = 0; i < numSyllables; ++i) {
			int r = rnd.nextInt(syllables.size());
			result += syllables.get(r) + "_";
		}
		return result;
	}

	public static void main(String[] args) {
		System.out.println(genPassword());
		System.out.println(genPassword());
		System.out.println(genPassword());
		System.out.println(genSyllablePassword());
		System.out.println(genSyllablePassword());
		System.out.println(genSyllablePassword());
	}

}
