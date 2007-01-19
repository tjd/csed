package passgen;

import java.util.ArrayList;
import java.util.Random;

public class RandomPassword {

	private static Random rnd = new Random();

	private final static String letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

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
	
	public static String genSyllablePassword() {
		ArrayList<String> syllables = new ArrayList<String>();
		syllables.add("Noo");
		syllables.add("Da");
		syllables.add("Urck");
		syllables.add("Slu");
		
		int numSyllables = 4 + rnd.nextInt(2);
		String result = "";
		for(int i = 0; i < numSyllables; ++i) {
			int r = rnd.nextInt(syllables.size());
			result = result + syllables.get(r);
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
