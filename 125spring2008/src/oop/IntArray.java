package oop;

import java.util.ArrayList;
import java.util.Random;

public class IntArray extends ArrayList<Integer> {
	
	private Random rnd;
	
	public IntArray() {
		rnd = new Random();
	}
	
	// Add values in the constructor, e.g.
	//   IntArray arr = new IntArray(4, 12, 0, 0, -9);
	public IntArray(Integer... args) {
		this();
		for(Integer x : args) {
			add(x);
		}
	}
	
	// remove and return a randomly chosen value
	public int removeRandom() {
		int r = rnd.nextInt(size());
		int x = remove(r);
		return x;
	}
	
	// demonstrates how you could modify a method to add 
	// a message for debugging purposes
	public boolean add(Integer x) {
		System.out.printf("%s added\n", x);
		// must call super.add to avoid an infinite loop
		return super.add(x);
	}
	
	// allow for negative index values as in Python
	public Integer remove(int i) {
		if (i < 0) {
			i = i + size();
		}
		// must call super.remove to avoid an infinite loop
		return super.remove(i);
	}

	public void print() {
		System.out.print(toString());
	}
	
	public void println() {
		System.out.println(toString());
	}
	
}
