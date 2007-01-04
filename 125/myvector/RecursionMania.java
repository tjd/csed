package myvector;

public class RecursionMania {

	public static int length(MyVector vec) {
		if (vec.isEmpty()) {
			return 0;
		} else {
			return 1 + length(vec.rest());
		}
	}
	
	public static void lengthTest() {
		MyVector vec1 = new MyVector();
		assert length(vec1) == 0;
		vec1.push(23);
		assert length(vec1) == 1;
		vec1.push(6);
		assert length(vec1) == 2;
		vec1.push(62);
		assert length(vec1) == 3;
	}
	
	public static void main(String[] args) {
		lengthTest();
		System.out.println("All test done.");
	}
	
}
