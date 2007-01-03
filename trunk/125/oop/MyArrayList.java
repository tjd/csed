package oop;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Random;

public class MyArrayList<T> extends ArrayList<T> {

	private static Random rnd = new Random();

	public MyArrayList() {
		super();
	}

	public MyArrayList(Collection<? extends T> arg0) {
		super(arg0);
	}

	public MyArrayList(int arg0) {
		super(arg0);
	}

	public T removeRandom() {
		int r = rnd.nextInt(size());
		return remove(r);
	}

	@Override
	public T remove(int i) {
		if (i < 0) {
			i += size();
		}
		return super.remove(i);
	}

	public static void main(String[] args) {
		MyArrayList<String> arr = new MyArrayList<String>();
		arr.add("cat");
		arr.add("dog");
		System.out.printf("%s\n", arr);
		String s = arr.removeRandom();
		System.out.printf("%s, %s", s, arr);

		ArrayList<String> arr2 = new MyArrayList<String>();
		// arr2.removeRandom(); // error

		// MyArrayList<String> arr3 = new ArrayList<String>();
	}
}
