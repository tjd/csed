package collections;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.TreeSet;

import util.ArrayList125;

public class Duplicates {

	public static void main(String[] args) {
//		ArrayList125<String> arr = new ArrayList125<String>("up", "up", "down");
		ArrayList125<String> arr = new ArrayList125<String>("up", "down",
				"all", "around", "down", "town", "Up", "and", "down", "around");

		System.out.printf("before: %s\nafter 1: %s\nafter 2: %s\nafter 3: %s", arr,
				removeDups1(arr), removeDups2(arr), removeDups3(arr));
	}

	public static ArrayList<String> removeDups1(ArrayList<String> arr) {
		ArrayList<String> result = new ArrayList<String>();
		for (String s : arr) {
			if (!result.contains(s)) {
				result.add(s);
			}
		}
		Collections.sort(result);  // for debugging purposes
		return result;
	}

	public static ArrayList<String> removeDups2(ArrayList<String> arr) {
		Collections.sort(arr);
		ArrayList<String> result = new ArrayList<String>();

		String lastOne = arr.get(0);
		result.add(lastOne);
		for (int i = 1; i < arr.size(); ++i) {
			while (arr.get(i).equals(lastOne)) {
				++i;
				if (i == arr.size()) {
					return result;
				}
			}
			lastOne = arr.get(i);
			result.add(lastOne);
		}
		return result;
	}
	
	public static ArrayList<String> removeDups3(ArrayList<String> arr) {
		TreeSet<String> result = new TreeSet<String>();
		for (String s : arr) {
			result.add(s);
		}
		return new ArrayList<String>(result);
	}
}
