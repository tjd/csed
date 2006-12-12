package pylist;

public class PyList {

	private int[] arr;

	private int last;

	public PyList() {
		this.arr = new int[10];
		this.last = 0;
	}

	public PyList(int[] arr) {
		this.arr = new int[arr.length];
		for (int i = 0; i < arr.length; ++i) {
			this.arr[i] = arr[i];
		}
	}

	private void doubleSize() {
		int[] result = new int[2 * this.arr.length];
		for (int i = 0; i < this.arr.length; ++i) {
			result[i] = this.arr[i];
		}
		this.arr = result;
	}

	public int size() {
		return this.last;
	}

	public int capacity() {
		return this.arr.length;
	}

	public int get(int i) {
		return this.arr[i];
	}

	public void set(int i, int val) {
		assert i < this.size();
		this.arr[i] = val;
	}

	public void append(int n) {
		if (this.size() == this.capacity()) {
			this.doubleSize();
		}
		this.arr[this.last] = n;
		++this.last;
	}

	public void reverse() {
		int a = 0;
		int b = this.size() - 1;
		while (a < b) {
			int temp = this.arr[a];
			this.arr[a] = this.arr[b];
			this.arr[b] = temp;
			++a;
			--b;
		}
	}

	public PyList copy() {
		return new PyList(this.arr);
	}

	@Override
	public boolean equals(Object x) {
		PyList other = (PyList) x;
		if (this.size() != other.size()) {
			return false;
		} else {
			for (int i = 0; i < this.size(); ++i) {
				if (this.arr[i] != other.arr[i]) {
					return false;
				}
			}
			return true;
		}
	}

	public String join(String sep) {
		if (this.size() == 1) {
			return "" + this.arr[0];
		} else {
			String result = "" + this.arr[0];
			for (int i = 1; i < this.size(); ++i) {
				result += sep + this.arr[i];
			}
			return result;
		}
	}

	@Override
	public String toString() {
		return "[" + this.join(", ") + "]";
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
