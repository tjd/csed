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
			List<Integer> rest = removeNegatives(arr.rest());
			rest.add(0, arr.first());
			return rest;
		}
	}
	
	public static void removeNegativesTest() {
		List<Integer> arr = new List<Integer>();
		List<Integer> result = removeNegatives(arr);
		assert result.isEmpty();
		arr.add(-354);
		result = removeNegatives(arr);
		assert result.isEmpty();
		arr.add(-9);
		result = removeNegatives(arr);
		assert result.isEmpty();
		arr.clear();
		arr.add(3);
		result = removeNegatives(arr);
		assert result.size() == 1 && result.get(0) == 3;
		arr.add(15);
		result = removeNegatives(arr);
		assert result.size() == 2 && result.get(0) == 3 && result.get(1) == 15;
		arr.add(0, -134);
		result = removeNegatives(arr);
		assert result.size() == 2 && result.get(0) == 3 && result.get(1) == 15;
	}
	
	public static void main(String[] args) {
		util.Util.ensureAssertionsEnabled();
		lengthTest();
		removeNegativesTest();
		System.out.println("All recursion tests passed.");
	}

}
