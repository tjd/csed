package nothanks;

import java.util.Collections;

public class IntArray extends List<Integer> {

	public IntArray() {
		super();
	}

	public int sum() {
		int sum = 0;
		for (Integer n : this) {
			sum += n;
		}
		return sum;
	}

	public void sort() {
		Collections.sort(this);
	}

	public void shuffle() {
		Collections.shuffle(this);
	}

	public int indexOfMax() {
		int p = 0;
		for (int i = 1; i < this.size(); ++i) {
			if (this.get(i) > this.get(p)) {
				p = i;
			}
		}
		return p;
	}
}
