package nothanks;

import java.util.Scanner;

public class HumanPlayer extends Player {

	@Override
	public String getName() {
		return "Hew Mon";
	}

	@Override
	public Move makeAmove(PlayerPot pot) {
		// show the user the current state of the game
		System.out.printf("\n***********************\n* Chips in the pot: %s\n* Up card: %s (%s remain)\n***********************\n", pot
				.getNumChips(), pot.getUpCard(), pot.getNumCards());
		pot.printPlayerCards();
		System.out.printf("Your chips: %s\nYour cards: %s\n\n", this
				.getNumChips(), this.getCards());

		Scanner sc = new Scanner(System.in);
		if (this.getNumChips() == 0) {
			System.out
					.printf("Uh oh! You have no chip left so you must take it!\n");
			System.out.println("Press 'enter' to continue ...");
			sc.nextLine();
			return Move.takeIt;
		} else {
			while (true) {
				System.out
						.printf("Please choose (T)ake it, or (N)o thanks --> ");
				String in = sc.next().trim().toUpperCase();
				if (in.length() > 0) {
					if (in.charAt(0) == 'T') {
						return Move.takeIt;
					} else if (in.charAt(0) == 'N') {
						return Move.noThanks;
					}
				}
				System.out.printf("Oops: I don't understand '%s'.\n", in);
			}
		}
	}

}
