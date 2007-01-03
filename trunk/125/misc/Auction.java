package misc;

public class Auction {

	public static void main(String[] args) {
		resolveAuction(new int[] { 6, 3, 1, 5, 2 });
		resolveAuction(new int[] { 18, 3, 53, 89 });
	}

	public static void resolveAuction(int[] bid) {
		assert bid.length > 1;

		// find the winner
		int winner = 0;
		for (int i = 1; i < bid.length; ++i) {
			if (bid[i] > bid[winner]) {
				winner = i;
			}
		}

		// now find the second-highest bid
		int bestSoFar = -1;
		for (int i = 0; i < bid.length; ++i) {
			if (i != winner) {
				if (bid[i] > bestSoFar) {
					bestSoFar = bid[i];
				}
			}
		}

		// print the results
		System.out.printf("Bidder %s wins, and pays %s\n", winner, bestSoFar);
	}

}
