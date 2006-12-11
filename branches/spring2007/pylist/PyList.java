package pylist;

public class PyList {

	private int[] arr;

	private int last;

	public PyList() {
		arr = new int[10];
		last = 0;
	}
	
	public PyList(int[] arr) {
		this.arr = new int[arr.length];
		for(int i = 0; i < arr.length; ++i) {
			this.arr[i] = arr[i];
		}
	}

	private void doubleSize() {
		int[] result = new int[2 * arr.length];
		for (int i = 0; i < arr.length; ++i) {
			result[i] = arr[i];
		}
		arr = result;
	}

	public int size() {
		return last;
	}

	public int capacity() {
		return arr.length;
	}
	
	public int get(int i) {
		return arr[i];
	}
	
	public void set(int i, int val) {
		assert i < size();
		arr[i] = val;
	}

	public void append(int n) {
		if (size() == capacity()) {
			doubleSize();
		}
		arr[last] = n;
		++last;
	}

	public void reverse() {
		int a = 0;
		int b = size() - 1;
		while (a < b) {
			int temp = arr[a];
			arr[a] = arr[b];
			arr[b] = temp;
			++a;
			--b;
		}
	}

	public PyList copy() {
		return new PyList(this.arr);
	}
	
	public boolean equals(Object x) {
		PyList other = (PyList) x;
		if (this.size() != other.size()) {
			return false;
		} else {
			for (int i = 0; i < size(); ++i) {
				if (this.arr[i] != other.arr[i]) {
					return false;
				}
			}
			return true;
		}
	}

	public String join(String sep) {
		if (size() == 1) {
			return "" + arr[0];
		} else {
			String result = "" + arr[0];
			for (int i = 1; i < size(); ++i) {
				result += sep + arr[i];
			}
			return result;
		}
	}

	public String toString() {
		return "[" + join(", ") + "]";
	}

	public static void main(String[] args) {
		PyList lst = new PyList();
		for (int i = 0; i < 20; ++i) {
			lst.append(i * i);
		}
		System.out.println("" + lst);
		lst.reverse();
		System.out.println("" + lst);
	}
	
}
