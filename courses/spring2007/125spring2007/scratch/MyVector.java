package scratch;

public class MyVector {
	private int[] arr;

	private int end;

	// default constructor
	public MyVector() {
		arr = new int[10];
		end = 0;
	}

	private void doubleSize() {
		int[] arr2 = new int[2 * arr.length];
		for (int i = 0; i < arr.length; ++i) {
			arr2[i] = arr[i];
		}
		arr = arr2;
	} // getters

	public int size() {
		return end;
	}

	public int capacity() {
		return arr.length;
	}

	public boolean isEmpty() {
		return size() == 0;
	}

	public int get(int i) {
		assert i < size();
		return arr[i];
	}

	public int first() {
		return get(0);
	}

	public MyVector rest() {
		assert size() > 0;
		MyVector result = new MyVector();
		for(int i = 1; i < size(); ++i) {
			result.push(get(i));
		}
		return result;
	}
	
	public int last() {
		return get(end - 1);
	}

	// mutators
	public void set(int i, int val) {
		assert i < size();
		arr[i] = val;
	}

	public void push(int val) {
		if (size() == capacity()) {
			doubleSize();
		}
		arr[end] = val;
		++end;
	}
	
	public String toString() {
		if (size() == 0) {
			return "[]";
		} else if (size() == 1) {
			return "[" + first() + "]";
		} else {
			// size() >= 2
			String result = "[" + first();
			for(int i = 1; i < end; ++i) {
				result += ", " + get(i);
			}
			return result + "]";
		}
	}

	public static void main(String[] args) {
		MyVector v = new MyVector();
		System.out.println("v = " + v);
		v.push(6);
		v.push(-14);
		System.out.println("v = " + v);
		v.push(5);
		v.push(-1);
		System.out.println("v = " + v);
	}
}
