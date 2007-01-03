package myvector;

public class MyVectorTest {
	public static void main(String[] args) {
		MyVector vec = new MyVector();
		assert vec.size() == 0;
		assert vec.toString().equals("[]");
		
		for(int i = 0; i < 10; ++i) {
			vec.push(i);
			assert vec.size() == (i + 1);
			assert vec.capacity() == 10;
		}
		
		assert vec.toString().equals("[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]") : vec.toString();
		
		vec.push(10);
		assert vec.size() == 11 : vec.size();
		assert vec.capacity() == 20;
		assert vec.toString().equals("[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]") : vec.toString();
		
		vec.push(11);
		assert vec.size() == 12 : vec.size();
		assert vec.capacity() == 20;
		assert vec.toString().equals("[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]") : vec.toString();
		
		assert vec.get(0) == 0;
		assert vec.get(1) == 1;
		assert vec.get(10) == 10;
		assert vec.get(11) == 11;
		assert vec.get(6) == 6;
		
		vec.set(0, -1);
		assert vec.get(0) == -1;
		
		vec.set(1, -1);
		assert vec.get(1) == -1;
		
		vec.set(10, -1);
		assert vec.get(10) == -1;
		
		vec.set(11, -1);
		assert vec.get(11) == -1;
		
		vec.set(6, -1);
		assert vec.get(6) == -1;
		
		System.out.println("all tests passed!");
	}
}
