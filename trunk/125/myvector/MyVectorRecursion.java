package myvector;

public class MyVectorRecursion {

	public static void main(String[] args) {
		lengthTest();
		sumTest();
		containsTest();
		indexOfTest();
		countTest();
		minTest();
		hasDuplicatesTest();
		System.out.println("All tests passed.");
	}
	
	public static int length(MyVector vec) {
		if (vec.isEmpty()) {
			return 0;
		} else {
			return 1 + length(vec.rest());
		}
	}
	
	public static void lengthTest() {
		MyVector vec = new MyVector();
		assert length(vec) == 0;
		vec.push(23);
		assert length(vec) == 1;
		vec.push(6);
		assert length(vec) == 2;
		vec.push(62);
		assert length(vec) == 3;
	}
	
	public static int sum(MyVector vec) {
		if (vec.isEmpty()) {
			return 0;
		} else {
			return vec.first() + sum(vec.rest());
		}
	}
	
	public static void sumTest() {
		MyVector vec = new MyVector();
		assert sum(vec) == 0;
		vec.push(23);
		assert sum(vec) == 23;
		vec.push(6);
		assert sum(vec) == 29;
		vec.push(62);
		assert sum(vec) == 91;
	}
	
	// linear search: returns true if n is in vec, and false otherwise
	public static boolean contains(MyVector vec, int n) {
		if (vec.isEmpty()) {
			return false;
		} else if (vec.first() == n) {
			return true;
		} else {
			return contains(vec.rest(), n);
		}
	}
	
	public static void containsTest() {
		MyVector vec = new MyVector();
		assert contains(vec, 12) == false;
		vec.push(23);
		assert contains(vec, 12) == false;
		assert contains(vec, 23) == true;
		vec.push(6);
		assert contains(vec, 12) == false;
		assert contains(vec, 23) == true;
		assert contains(vec, 6) == true;
		vec.push(-43);
		assert contains(vec, 12) == false;
		assert contains(vec, 23) == true;
		assert contains(vec, 6) == true;
		assert contains(vec, -43) == true;
	}
	
	// if n is in vec, returns some i such that vec.get(i) == n;
	// otherwise, if n is not in vec, returns -1
	public static int indexOf(MyVector vec, int n) {
		return indexOf(vec, n, 0);
	}
	
	// helper methods for indexOf: includes an extra parameter (start)
	// that keeps track of how many elements have been checked
	public static int indexOf(MyVector vec, int n, int start) {
		if (vec.isEmpty()) {
			return -1;
		} else if (vec.first() == n) {
			return start;
		} else {
			return indexOf(vec.rest(), n, start + 1);
		}
	}
	
	public static void indexOfTest() {
		MyVector vec = new MyVector();
		assert indexOf(vec, 12) == -1;
		vec.push(23);
		assert indexOf(vec, 12) == -1;
		assert indexOf(vec, 23) == 0;
		vec.push(6);
		assert indexOf(vec, 12) == -1;
		assert indexOf(vec, 23) == 0;
		assert indexOf(vec, 6) == 1;
		vec.push(-43);
		assert indexOf(vec, 12) == -1;
		assert indexOf(vec, 23) == 0;
		assert indexOf(vec, 6) == 1;
		assert indexOf(vec, -43) == 2;
	}
	
	// returns the number of times n occurs in vec
	public static int count(MyVector vec, int n) {
		if (vec.isEmpty()) {
			return 0;
		} else if (vec.first() == n) {
			return 1 + count(vec.rest(), n);
		} else {
			return count(vec.rest(), n);
		}
	}
	
	public static void countTest() {
		MyVector vec = new MyVector();
		assert count(vec, 2) == 0;
		vec.push(8);
		assert count(vec, 2) == 0;
		assert count(vec, 8) == 1;
		vec.push(5);
		assert count(vec, 2) == 0;
		assert count(vec, 8) == 1;
		assert count(vec, 5) == 1;
		vec.push(8);
		assert count(vec, 2) == 0;
		assert count(vec, 8) == 2;
		assert count(vec, 5) == 1;
		vec.push(8);
		assert count(vec, 2) == 0;
		assert count(vec, 8) == 3;
		assert count(vec, 5) == 1;
		vec.push(5);
		assert count(vec, 2) == 0;
		assert count(vec, 8) == 3;
		assert count(vec, 5) == 2;
	}
	
	// returns the smallest of two ints
	public static int min(int x, int y) {
		if (x < y) {
			return x;
		} else {
			return y;
		}
	}
	
	// returns the smallest element in vec
	public static int min(MyVector vec) {
		assert vec.size() >= 1;
		if (vec.size() == 1) {
			return vec.first();
		} else {
			return min(vec.first(), min(vec.rest()));
		}
	}
	
	public static void minTest() {
		MyVector vec = new MyVector();
		vec.push(40);
		assert min(vec) == 40;
		vec.push(50);
		assert min(vec) == 40;
		vec.push(30);
		assert min(vec) == 30;
		vec.push(80);
		assert min(vec) == 30;
	}
	
	public static boolean hasDuplicates(MyVector vec) {
		if (vec.isEmpty()) {
			return false;
		} else {
			int first = vec.first();
			MyVector rest = vec.rest();
			if (contains(rest, first)) {
				return true;
			} else {
				return hasDuplicates(rest);
			}
		}
	}
	
	public static void hasDuplicatesTest() {
		MyVector vec = new MyVector();
		assert hasDuplicates(vec) == false;
		vec.push(34);
		assert hasDuplicates(vec) == false;
		vec.push(9);
		assert hasDuplicates(vec) == false;
		vec.push(22);
		assert hasDuplicates(vec) == false;
		vec.push(34);
		assert hasDuplicates(vec) == true;
		vec.push(34);
		assert hasDuplicates(vec) == true;
		vec.push(7);
		assert hasDuplicates(vec) == true;
		vec.push(9);
		assert hasDuplicates(vec) == true;
		MyVector vec2 = new MyVector();
		vec2.push(5);
		vec2.push(5);
		assert hasDuplicates(vec2) == true;
	}	
}
