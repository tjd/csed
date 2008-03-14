package formcount;

import java.util.ArrayList;
import java.util.Collections;

public class FormCounter {

	public static void main(String[] args) {
		final int NUM_PEOPLE = 100000;
	    int formCount = 1;
		ArrayList<Integer> people = makeRandomList(NUM_PEOPLE);
		
		int indexOfTallestSoFar = 0;
		for(int i = 1; i < NUM_PEOPLE; ++i) {
			if (people.get(i) > people.get(indexOfTallestSoFar)) {
				indexOfTallestSoFar = i;
				++formCount;
			}
		}
		System.out.printf("%s people lined up\n", NUM_PEOPLE);
		System.out.printf("%s forms were used\n", formCount);
	}
	
	public static ArrayList<Integer> makeRandomList(int n) {
		ArrayList<Integer> arr = new ArrayList<Integer>();
		for(int i = 0; i < n; ++i) {
			arr.add( i);
		}
		Collections.shuffle(arr);
		return arr;
	}

}
