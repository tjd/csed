package recursionlab;

import myarraylist.List;

public class Recursion {

	public static int length(List<Integer> arr) // accessor of length
	{
		if (arr.isEmpty()) {
			return 0;
		} else {
			return 1 + length(arr.rest());
		}
	}

	public static int sum(List<Integer> arr) // accessor of sum
	{
		if (arr.isEmpty())
			return 0;
		else
			return arr.first() + sum(arr.rest());

	}

	public static int recMin(List<Integer> arr) {
		if (arr.size() == 1) {
			return arr.first();
		} else {
			return Math.min(arr.first(), recMin(arr.rest()));
		}
	}

	public static int recMax(List<Integer> arr) {
		if (arr.size() == 1) {
			return arr.first();
		} else {
			return Math.max(arr.first(), recMax(arr.rest()));
		}
	}


	public static int count(List<Integer> arr, int x) {
		if (arr.isEmpty()) {
			return 0;
		} else if (arr.first() == x) {
			return 1 + count(arr.rest(), x);
		} else {
			return count(arr.rest(), x);
		}
	}

	public static boolean isSortedAscending(List<Integer> arr) {
		if (arr.size() <= 1) {
			return true;
		} else if (arr.get(0) > arr.get(1)) { // out of order?
			return false;
		} else {
			return isSortedAscending(arr.rest());
		}
	}

	public static boolean isSortedDescending(List<Integer> arr) {
		if (arr.size() <= 1) {
			return true;
		} else if (arr.get(0) < arr.get(1)) { // out of order?
			return false;
		} else {
			return isSortedDescending(arr.rest());
		}
	}

	public static boolean same(List<Integer> A, List<Integer> B) {
		if (A.size() != B.size()) {
			return false;
		} else if (A.first() != B.first()) {
			return false;
		} else {
			return same(A.rest(), B.rest());
		}
	}


	public static String expand(String acronym, String expansion, int n) 
	{
		if (n == 1) {
			return expansion;
		} else {
			String exp = expand(acronym, expansion, n - 1);
			return exp.replaceAll(acronym, expansion);
		}
	}

}

