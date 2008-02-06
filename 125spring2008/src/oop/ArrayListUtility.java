package oop;

import java.util.ArrayList;

public class ArrayListUtility {

	public static String longestString(ArrayList<String> arr) {
		String longestSoFar = arr.get(0);
		for (int i = 1; i < arr.size(); ++i) {
			if (arr.get(i).length() > longestSoFar.length()) {
				longestSoFar = arr.get(i);
			}
		}
		return longestSoFar;
	}

	public static void main(String[] args) {
		ArrayList<String> names = new ArrayList<String>();
		names.add("Jack");
		names.add("Jill");
		names.add("Moe");
		names.add("George");

		String longest = longestString(names);
		System.out.printf("Longest name: %s", longest);
	}

}
