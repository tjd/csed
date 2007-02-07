package recursion;

import java.util.Random;

public class QuicksortTest {

	public static final boolean DEBUG = false;
	
	public static void debug(String s) {
		if (DEBUG) {
			System.out.print(s);
		}
	}
	
	public static int partition(MyArray arr, int begin, int end) {
			int p = begin;
			int pivotValue = arr.get(begin);
			for (int i = begin + 1; i <= end; ++i) {
				int value = arr.get(i);
				if (value <= pivotValue) {
					// swap that values at i, p
					arr.swap(i, p + 1);
					arr.swap(p, p + 1);
					++p;
				}
			}
			
			debug(String.format("p=%s, end=%s", p, end));

			return p;
	}

	public static void quicksort(MyArray arr, int begin, int end) {
		debug(String.format("quicksort(%s, %s, %s)\n", arr, begin, end));
		if (begin < end) {
			int p = partition(arr, begin, end);
			quicksort(arr, begin, p - 1);
			quicksort(arr, p + 1, end);
		}
	}

	public static void quicksort(MyArray arr) {
		quicksort(arr, 0, arr.size() - 1);
	}

	public static boolean isSorted(MyArray arr, int begin, int end) {
		for (int i = begin + 1; i <= end; ++i) {
			if (arr.get(i - 1) > arr.get(i)) {
				return false;
			}
		}
		return true;
	}

	public static boolean isSorted(MyArray arr) {
		return isSorted(arr, 0, arr.size() - 1);
	}

	
	private static Random rnd = new Random();
	
	public static MyArray randArray(int n) {
		MyArray arr = new MyArray();
		for(int i = 0; i < n; ++i) {
			arr.add(rnd.nextInt());
		}
		return arr;
	}
	
	public static void quicksortTest() {
		MyArray arr = new MyArray();
		quicksort(arr);
		assert isSorted(arr);
		arr.add(5);
		quicksort(arr);
		assert isSorted(arr);
		arr.add(2);
		quicksort(arr);
		assert isSorted(arr);
		arr.add(6);
		quicksort(arr);
		assert isSorted(arr);
		arr.add(1);
		quicksort(arr);
		assert isSorted(arr) : arr;
		arr.add(3);
		quicksort(arr);
		assert isSorted(arr);
		arr = randArray(1000);
		quicksort(arr);
		assert isSorted(arr);
	}

	public static void main(String[] args) {
		util.Util.ensureAssertionsEnabled();
		quicksortTest();
		System.out.println("All quicksort tests passed.");
	}

}
