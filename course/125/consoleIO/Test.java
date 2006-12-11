package consoleIO;

import java.util.Scanner;

public class Test {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		double d = 1.0 / 3;
		System.out.println("d = " + d);
		Scanner sc = new Scanner(System.in);
		System.out.printf("Enter a line of text: ");
		String line = sc.nextLine();
		System.out.printf("You entered \"%s\"", line);
	}

}
