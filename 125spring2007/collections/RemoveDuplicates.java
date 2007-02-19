package collections;

import java.util.HashSet;

public class RemoveDuplicates {

	public static void main(String[] args) {
		String[] arr = { "cat", "cat", "dog", "bird", "dog", "mouse", "bird",
				"fish", "cat" };
		HashSet<String> set = new HashSet<String>();

		for (String element : arr) {
			set.add(element);
		}

		System.out.printf("set = %s\n", set);
	}

}
