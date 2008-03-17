import java.util.Arrays;

public class SelectionSort {

	public static void main(String[] args) {
//		int a = 5;
//		int b = 7;
//		System.out.printf("a = %s  b = %s\n", a, b);
//		swap(a, b);
//		System.out.printf("a = %s  b = %s\n", a, b);
		int[] arr = {65, 9, 0, -3, 2, 80};
		System.out.printf("arr = %s\n", Arrays.toString(arr));
		selectionSort(arr);
		System.out.printf("arr = %s\n", Arrays.toString(arr));
	}

//	public static void swap(int x, int y) {
//		int temp = x;
//		x = y;
//		y = temp;
//	}

	public static void selectionSort(int[] arr) {
		for (int i = 0; i < arr.length; ++i) {
			int m = indexOfMin(i, arr);
			// swap m and i value
			int temp = arr[m];
			arr[m] = arr[i];
			arr[i] = temp;
		}
	}

	public static int indexOfMin(int start, int[] arr) {
		int indexOfMinSoFar = start;
		for (int i = start + 1; i < arr.length; ++i) {
			if (arr[i] < arr[indexOfMinSoFar]) {
				indexOfMinSoFar = i;
			}
		}
		return indexOfMinSoFar;
	}

}
