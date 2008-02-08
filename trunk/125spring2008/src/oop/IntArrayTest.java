package oop;

public class IntArrayTest {

	public static void main(String[] args) {
		IntArray arr = new IntArray(3, 4, 5, 6, 7);
		arr.println();
		int x = arr.removeRandom();
		arr.println();
		System.out.printf("%s removed\n\n", x);
		
		int last = arr.remove(-1);
		arr.println();
		System.out.printf("%s removed\n", last);
	}

}
