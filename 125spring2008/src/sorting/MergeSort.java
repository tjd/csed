package sorting;

import java.util.ArrayList;
import java.util.Arrays;

public class MergeSort {

	// returns the first element of arr
	public static int first(int[] arr) {
		return arr[0];
	}

	// returns a new array that contains all the elements of arr except for the
	// first
	public static int[] rest(int[] arr) {
		return copy(arr, 1, arr.length);
	}

	public static int[] copy(int[] arr, int begin, int end) {
		int n = end - begin;
		int[] result = new int[n];
		for (int i = begin; i < end; ++i) {
			result[i - begin] = arr[i];
		}
		return result;
	}

	public static boolean isSorted(int[] arr) {
		if (arr.length == 1) {
			return true;
		} else {
			int[] tail = rest(arr);
			return (first(arr) <= first(tail)) && isSorted(tail);
		}
	}

	public static void isSortedTest() {
		assert isSorted(new int[] { 5 });
		assert isSorted(new int[] { 2, 5 });
		assert isSorted(new int[] { 2, 5, 9, 44 });
		assert !isSorted(new int[] { 5, 4 });
		assert !isSorted(new int[] { 5, 6, 7, 9, 8, 10 });
		System.out.printf("all isSortedTest tests passed\n");
	}

	public static int[] merge(int[] A, int B[]) {
		assert isSorted(A);
		assert isSorted(B);
		int[] result = new int[A.length + B.length];
		int a = 0, b = 0;
		for (int i = 0; i < result.length; ++i) {
			if (a == A.length) {
				result[i] = B[b];
				++b;
			} else if (b == B.length) {
				result[i] = A[a];
				++a;
			} else if (A[a] < B[b]) {
				result[i] = A[a];
				++a;
			} else {
				result[i] = B[b];
				++b;
			}
		}
		return result;
	}

	public static int[] mergeSort(int[] arr) {
		if (arr.length <= 1) {
			return new int[] { arr[0] };
		} else {
			int mid = arr.length / 2;
			int[] left = copy(arr, 0, mid);
			int[] right = copy(arr, mid, arr.length);
			left = mergeSort(left);
			right = mergeSort(right);
			return merge(left, right);
		}
	}
	
	public static ArrayList<Integer> mergeSort(ArrayList<Integer> arr) {
		Integer[] temp = arr.toArray(new Integer[] {});
		ArrayList<Integer> result = new ArrayList<Integer>(temp.length);
		for(Integer x : temp) {
			result.add(x);
		}
		return result;
	}

	public static void mergeSortTest() {
		assert isSorted(mergeSort(new int[] { 6 }));
		assert isSorted(mergeSort(new int[] { 2, 4 }));
		assert isSorted(mergeSort(new int[] { 4, 2 }));
		assert isSorted(mergeSort(new int[] { 12, -4, 15, 7, 1, 2 }));
		assert isSorted(mergeSort(new int[] { 12, -4, 15, 7, 1, 3, 2 }));
		System.out.printf("%s\n", Arrays.toString(mergeSort(new int[] { 12, -4,
				15, 7, 1, 3, 2 })));
		System.out.printf("all mergeSortTest tests passed\n");
	}

	public static void main(String[] args) {
		isSortedTest();
		mergeSortTest();
	}

}

