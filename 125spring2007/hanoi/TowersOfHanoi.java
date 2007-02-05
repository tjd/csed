package hanoi;

/*
 * Towers of Hanoi solver based on Listing 11.4 from LL5.
 */

public class TowersOfHanoi {

	private int totalDisks;
 
	private int moveCount;

	public TowersOfHanoi(int n) {
		this.totalDisks = n;
	}

	public void solve() {
		moveCount = 0;
		moveTower(totalDisks, 1, 3, 2);
	}

	private void moveTower(int numDisks, int start, int end, int helper) {
		if (numDisks == 1) {
			moveOneDisk(start, end);
		} else {
			moveTower(numDisks - 1, start, helper, end);
			moveOneDisk(start, end);
			moveTower(numDisks - 1, helper, end, start);
		}
	}

	private void moveOneDisk(int start, int end) {
		++moveCount;
		System.out.printf("%s. Move one disk from %s to %s\n", moveCount,
				start, end);
	}

}
