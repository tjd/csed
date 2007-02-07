package recursion;

public class QuicksortTest {

	public static final boolean DEBUG = false;
	
	public static void debug(String s) {
		if (DEBUG) {
			System.out.print(s);
		}
	}
	
	public static int partition(MyArray arr, int begin, int end) {
		if (begin < end) {
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

			assert isPartitioned(arr, p, begin, end) : String.format(
					"%s, begin = %s, bigsStartAt = %s, end = %s", arr, begin,
					p, end);
			return p;
		} else {
			return begin;
		}
	}

	public static int min(MyArray arr, int begin, int end) {
		int m = arr.get(begin);
		for (int i = begin + 1; i <= end; ++i) {
			if (arr.get(i) < m) {
				m = arr.get(i);
			}
		}
		return m;
	}

	public static int max(MyArray arr, int begin, int end) {
		int m = arr.get(begin);
		for (int i = begin + 1; i <= end; ++i) {
			if (arr.get(i) > m) {
				m = arr.get(i);
			}
		}
		return m;
	}

	public static boolean isPartitioned(MyArray arr, int p, int begin, int end) {
		return p == begin || p == end
				|| max(arr, begin, p - 1) < min(arr, p, end);
	}

	public static void partitionTest() {
		MyArray arr = new MyArray();
		arr.add(1);
		arr.add(2);
		assert isPartitioned(arr, partition(arr, 0, 1), 0, 1);
		arr.add(7);
		assert isPartitioned(arr, partition(arr, 0, 2), 0, 2);
		arr.add(5);
		assert isPartitioned(arr, partition(arr, 0, 3), 0, 3);
		arr.add(0, 4);
		assert isPartitioned(arr, partition(arr, 0, 4), 0, 4);
		arr.add(3);
		assert isPartitioned(arr, partition(arr, 0, 5), 0, 5);
		arr = new MyArray();
		arr.add(2);
		arr.add(5);
		arr.add(6);
		arr.add(1);
		assert isPartitioned(arr, partition(arr, 0, 3), 0, 3);
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
	}

	public static void main(String[] args) {
		util.Util.ensureAssertionsEnabled();
		partitionTest();
		quicksortTest();
		System.out.println("All quicksort tests passed.");
	}

}
