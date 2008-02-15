package misc;

import java.util.ArrayList;

public class Eq {


	public static void main(String[] args) {
		int a = new Integer(5);  // unboxing
		Integer b = 5; // boxing
		
		if (b.equals(a)) {
			System.out.println("same!");
		} else {
			System.out.println("different!");
		}
		
		ArrayList<Integer> arr = new ArrayList<Integer>();
		arr.add(5);
		int x = arr.get(0);
		
//		String s = "cat";
//		String t = "cat";
//		if (s.equals(t)) {
//			System.out.println("same!");
//		} else {
//			System.out.println("different!");
//		}
	}

}
