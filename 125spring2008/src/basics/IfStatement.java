package basics;

import java.util.Scanner;

public class IfStatement {

	public static void main(String[] args) {
		Scanner sc = new Scanner(System.in);

		System.out.printf("What is your password Mr. President? ");
		String password = sc.next();

		if (password.equals("cooldood")) {
			System.out
					.printf("\nThat is the right password: Good memory Mr. President!\n");
			System.out.printf("How many hot dogs would you like for lunch? ");
			int numHotDogs = sc.nextInt();
			if (numHotDogs <= 0) {
				System.out.printf("Not hungry today sir?");
			} else if (numHotDogs <= 25) {
				System.out.printf("\ns tasty hot dogs have been ordered.",
						numHotDogs);
			} else {
				System.out
						.printf("\nGee, do you think it is a good idea to eat that many "
								+ "hot dogs? Remember what happened last time!");
			}
		} else {
			System.out.printf("Incorrect password.");
		}
	}

}
