package recursion;

import java.util.ArrayList;

public class MyArray extends ArrayList<Integer> {
	public void swap(int i, int j) {
		int temp = this.get(i);
		this.set(i, this.get(j));
		this.set(j, temp);
	}
}
