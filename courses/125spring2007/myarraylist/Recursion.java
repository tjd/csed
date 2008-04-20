package myarraylist;

public class Recursion {

	public static int length(List<Integer> arr) {
		if (arr.isEmpty()) {
			return 0;
		} else {
			return 1 + length(arr.rest());
		}
	}

	public static void lengthTest() {
		// List<Integer> lst = new List<Integer>();
		// lst.add(34);
		// lst.add(348);
		// lst.add(22);
		// System.out.println("length of " + lst + " is " + length(lst));
		List<Integer> arr = new List<Integer>();
		assert length(arr) == 0;
		arr.add(1);
		assert length(arr) == 1;
		arr.add(5);
		assert length(arr) == 2;
		arr.add(-34);
		assert length(arr) == 3;
	}

	// returns a copy of arr with all negative values removed
	public static List<Integer> removeNegatives(List<Integer> arr) {
		if (arr.isEmpty()) {
			return new List<Integer>();
		} else if (arr.first() < 0) {
			return removeNegatives(arr.rest());
		} else {
			List<Integer> R = removeNegatives(arr.rest());
			R.add(0, arr.first());
			return R;
		}
	}

	public static int minList(List<Integer> arr) {
		assert !arr.isEmpty();
		if (arr.size() == 1) {
			return arr.get(0);
		} else {
			return Math.min(arr.first(), minList(arr.rest()));
		}
	}

	public static void minListTest() {
		List<Integer> arr = new List<Integer>();
		arr.add(5);
		assert minList(arr) == 5;
		arr.add(6);
		assert minList(arr) == 5;
		arr.add(-34);
		assert minList(arr) == -34;
	}

	public static boolean linSearch(int x, List<Integer> arr) {
		if (arr.isEmpty()) {
			return false;
		} else if (arr.first() == x) {
			return true;
		} else {
			return linSearch(x, arr.rest());
		}
	}

	public static void linSearchTest() {
		List<Integer> arr = new List<Integer>();
		assert linSearch(2, arr) == false;
		arr.add(5);
		assert linSearch(2, arr) == false;
		assert linSearch(5, arr) == true;
		assert linSearch(15, arr) == false;
		arr.add(6);
		assert linSearch(2, arr) == false;
		assert linSearch(5, arr) == true;
		assert linSearch(6, arr) == true;
		assert linSearch(15, arr) == false;
	}

	public static void main(String[] args) {
		util.Util.ensureAssertionsEnabled();
		lengthTest();
		minListTest();
		linSearchTest();
		System.out.println("All recursion tests passed.");
	}

}
