package myvector;

public class MyVector {
	
	private int[] arr;
	private int end;
	
	// doubles the length of the underlying array
	private void doubleSize() {
		int[] arr2 = new int[2 * arr.length];
		for(int i = 0; i < arr.length; ++i) {
			arr2[i] = arr[i];
		}
		arr = arr2;
	}
	
	public MyVector() {
		arr = new int[10];
		end = 0;
	}
	
	public int size() {
		return end;	
	}
	
	public int capacity() {
		return arr.length;
	}

	// append x to the end of the vector; automatically increases
	// capacity if necessary
	public void push(int x) {
		if (size() == capacity()) {
			doubleSize();
		}
		arr[end] = x;
		++end;
	}
	
	// returns value at location i
	public int get(int i) {
		if (i < size()) {
			return arr[i];
		} else {
			throw new RuntimeException(String.format("MyVector has size %s; cannot get location %s", 
	                   size(), i));
}
	}
	
	// sets location i to be value
	public void set(int i, int value) {
		if (i < size()) {
			arr[i] = value;
		} else {
			throw new RuntimeException(String.format("MyVector has size %s; cannot set location %s", 
					                   size(), i));
		}
	}
	
	public String toString() {
		if (size() == 0) {
			return "[]";
		} else if (size() == 1) {
			return "[" + arr[0] + "]";
		} else {
			// size() >= 2
			String result = "[" + arr[0];
			for(int i = 1; i < end; ++i) {
				result += ", " + arr[i];
			}
			return result + "]";
		}
	}
}
