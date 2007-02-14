package recrusionlab;

import myarraylist.List;

public class RecursionTest {
	public static void main(String[] args) {
		util.Util.ensureAssertionsEnabled();
		lengthTest();
		sumTest();
		System.out.println("all recursion tests passed");
	}

	public static void lengthTest() {
		List<Integer> arr = new List<Integer>();
		assert Recursion.length(arr) == 0;
		arr.add(3);
		assert Recursion.length(arr) == 1;
		arr.add(4);
		assert Recursion.length(arr) == 2;
	}

	public static void sumTest() {
		List<Integer> arr = new List<Integer>();
		assert Recursion.sum(arr) == 0;
		arr.add(3);
		assert Recursion.sum(arr) == 3;
		arr.add(4);
		assert Recursion.sum(arr) == 7;
	}
}
