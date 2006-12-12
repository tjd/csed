package alg;

/*
 * 
 * Search.java
 * 
 * Examples of basic Java searching functions.
 * 
 * Questions
 * 
 *  - Write the min and indexOfMin methods.
 *  
 *  - Modify the indexOfMax to return the right-most occurrence of the maximum value.
 *  
 *  - Write a method with the following header:
 *  
 *        // Returns the number of times x appears in arr
 *        public int count(int x, int[] arr)
 * 
 *  - Show how to write the search method using the count method from the previous question.
 *  
 *  - Write a version of index that starts at a given index value, searches to the right, 
 *    and then wraps-around to the beginning of the array and keeps searching if the end of 
 *    the array is hit before finding the target.
 *    
 *  - Write a version of index that begins searching at a given index location, and then
 *    expands the search outwards in both the left and right direction. The advantage of this
 *    algorithm is that it will return the matching target that is
 *    closest to the point where the search started.
 *  
 */

public class Search {

	// Returns the index of the first (left-most) occurrence of
	// x in arr. If x is not arr, -1 is returned.
	public static int index(int x, int[] arr) {
		for (int i = 0; i < arr.length; ++i) {
			if (arr[i] == x) {
				return i;
			}
		}
		return -1;
	}

	// Returns true iff x occurs one or more times in arr.
	public static boolean contains(int x, int[] arr) {
		return index(x, arr) != -1;
	}

	// Returns the greatest value in arr.
	public static int max(int[] arr) {
		int maxSoFar = arr[0];
		for (Integer val : arr) {
			if (val > maxSoFar) {
				maxSoFar = val;
			}
		}
		return maxSoFar;
	}

	// Returns the index of the left-most occurrence of the maximum
	// value in arr.
	public static int indexOfMax(int[] arr) {
		int maxIndexSoFar = 0;
		for (int i = 1; i < arr.length; ++i) {
			if (arr[i] > arr[maxIndexSoFar]) {
				maxIndexSoFar = i;
			}
		}
		return maxIndexSoFar;
	}

	public static void test1() {
		int[] arr = { 3, 4, 1, -4, 3, 2 };
		assert index(1, arr) == 2;
		assert index(3, arr) == 0;
		assert index(2, arr) == 5;
		assert index(10, arr) == -1;

		assert contains(3, arr);
		assert contains(2, arr);
		assert !contains(5, arr);

		assert max(arr) == 4;

		assert indexOfMax(arr) == 1;
		System.out.printf("done\n");
	}

	public static void main(String[] args) {
		test1();
	}

}
