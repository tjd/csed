package sorting;

import java.util.Arrays;

public class SelectionSort {
	// Sorts arr into ascending order using selection sort.
	public static void selectionSort(int[] arr) {
		if (arr.length <= 1) {
			return;
		} else {
			for (int i = 0; i < arr.length; ++i) {
				int p = indexOfMin(arr, i);
				// swap arr[i] with arr[p]
				int temp = arr[i];
				arr[i] = arr[p];
				arr[p] = temp;
			}
		}
	}

	// Returns the index of the left-most occurrence of the minimum
	// value in arr, starting at begin.
	public static int indexOfMin(int[] arr, int begin) {
		int minIndexSoFar = begin;
		for (int i = begin + 1; i < arr.length; ++i) {
			if (arr[i] < arr[minIndexSoFar]) {
				minIndexSoFar = i;
			}
		}
		return minIndexSoFar;
	}

	// Reverses the elements of arr.
	public static void reverse(int[] arr) {
		int a = 0;
		int b = arr.length - 1;
		while (a < b) {
			int temp = arr[a];
			arr[a] = arr[b];
			arr[b] = temp;
			++a;
			--b;
		}
	}

	// Sort arr into descending order.
	public static void reverseSelectionSort(int[] arr) {
		selectionSort(arr);
		reverse(arr);
	}

	// Checks if arr is in sorted order, from smallest to largest.
	public static boolean isNonDescending(int[] arr) {
		for (int i = 1; i < arr.length; ++i) {
			if (arr[i - 1] > arr[i]) {
				return false;
			}
		}
		return true;
	}

	// Checks if arr is in sorted order, from largest to smallest.
	public static boolean isNonAscending(int[] arr) {
		for (int i = 1; i < arr.length; ++i) {
			if (arr[i - 1] < arr[i]) {
				return false;
			}
		}
		return true;
	}

	public static void main(String[] args) {
		int[] arr = { 4, 2, 6, 78, 3, 34, 6, 6, 2, 1 };
		selectionSort(arr);
		System.out.printf("%s\n", Arrays.toString(arr));
		assert isNonDescending(arr);
		arr = new int[] { 4, 2, 6, 78, 3, 34, 6, 6, 2, 1 };
		reverseSelectionSort(arr);
		System.out.printf("%s", Arrays.toString(arr));
		assert isNonAscending(arr);
	}
}
