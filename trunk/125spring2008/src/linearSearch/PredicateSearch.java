package linearSearch;

import java.util.ArrayList;
import util.ArrayList125;

public class PredicateSearch {

	public static void main(String[] args) {
		ArrayList125<Integer> arr = new ArrayList125<Integer>(5, 4, 2, 1, 3, 7);
		assert find(new MyPred(), arr) == -1;
		assert find(new MyOtherPred(), arr) == 0;

		// using an anonymous class
		assert find(new Predicate() {
			public boolean f(int x) {
				return x == 2 || x == 3;
			}
		}, arr) == 2;
		System.out.printf("All done!\n");
	}

	static int find(Predicate pred, ArrayList<Integer> arr) {
		for (int i = 0; i < arr.size(); ++i) {
			if (pred.f(arr.get(i))) {
				return i;
			}
		}
		return -1;
	}

}
