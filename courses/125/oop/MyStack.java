package oop;

/*
 * A dynamic stack class.
 */

public class MyStack<T> {

	private Object[] arr;

	private int next;

	public MyStack() {
		arr = new Object[10];
		next = 0;
	}

	private void doubleInSize() {
		// make a new array twice the size of the current one
		Object[] newArr = new Object[capacity() * 2];

		// copy the current array into the new one
		for (int i = 0; i < size(); i++) {
			newArr[i] = arr[i];
		}

		// point to the new array
		arr = newArr;
	}

	// returns the number of elements stored in the array
	public int size() {
		return next;
	}

	public boolean isEmpty() {
		return size() == 0;
	}

	// returns the total number of slots in the array
	public int capacity() {
		return arr.length;
	}

	// appends x to the end of the array, re-sizing it if necessary
	public void push(T x) {
		if (size() == capacity()) {
			doubleInSize();
		}
		arr[next] = x;
		++next;
	}

	// returns and removes the last element on arr; if arr is empty,
	// an exception is thrown
	public T pop() {
		if (size() == 0) {
			throw new Error("Cannot pop empty stack.");
		} else {
			--next;
			return (T) arr[next];
		}
	}

	public void print() {
		if (size() == 0) {
			System.out.print("{}");
		} else if (size() == 1) {
			System.out.printf("{%s}", arr[0]);
		} else {
			System.out.printf("{%s", arr[0]);
			for (int i = 1; i < size(); i++) {
				System.out.printf(", %s", arr[i]);
			}
			System.out.print("}");
		}
	}

	public void println() {
		print();
		System.out.println("");
	}

}
