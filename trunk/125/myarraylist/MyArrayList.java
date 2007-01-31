package myarraylist;

import java.util.ArrayList;

public class MyArrayList<T> extends ArrayList<T> {

	public MyArrayList() {
		super();
	}

	// Returns a reference to the first element of this MyArrayList.
	public T first() {
		return this.get(0);
	}

	// Returns a new MyArrayList the same as this one,
	// but without the first element.
	public MyArrayList<T> rest() {
		MyArrayList<T> arr = new MyArrayList<T>();
		for (int i = 1; i < this.size(); ++i) {
			arr.add(this.get(i));
		}
		return arr;
	}

	public static void main(String[] args) {
		MyArrayList<Integer> ai = new MyArrayList<Integer>();
		ai.add(5);
		System.out.printf("%s, first = %s, rest = %s\n", ai, ai.first(), ai.rest());
		ai.add(9);
		System.out.printf("%s, first = %s, rest = %s\n", ai, ai.first(), ai.rest());

		MyArrayList<String> as = new MyArrayList<String>();
		as.add("cow");
		System.out.printf("%s, first = %s, rest = %s\n", as, as.first(), as.rest());
		as.add("moose");
		System.out.printf("%s, first = %s, rest = %s\n", as, as.first(), as.rest());
	}
}
