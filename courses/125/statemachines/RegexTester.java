package statemachines;

import java.util.Scanner;

public class RegexTester {

	public static final String regex = "\\s*(\\(\\d{3}\\) ?)?\\d{3}[- ]\\d{4}\\s*";
	
	// identifier: "[a-zA-Z_][a-zA-Z_0-9]*"
	// integers: "-?[0-9]+"
	// integers (no leading 0s): "0|(-?[1-9][0-9]*)"
	// postal code: "[A-Z][0-9][A-Z][0-9][A-Z][0-9]"
	// postal code: "[A-Z]\\d[A-Z] ?\\d[A-Z]\\d"
	// phone number: "\\d\\d\\d-\\d\\d\\d\\d"
	// phone number: "\\d{3}-\\d{4}"
	// phone number: "\\s*(\\(\\d{3}\\) ?)?\\d{3}[- ]\\d{4}\\s*"

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Scanner scan = new Scanner(System.in);
		System.out.printf("Enter string --> ");
		while (true) {
			String s = scan.nextLine();
			if (s.matches("quit|done|stop")) {
				System.out.printf("done!");
				return;
			} else if (s.matches(regex)) {
				System.out.printf("'%s' matches '%s'\n\n", s, regex);
			} else {
				System.out.printf("'%s' DOES NOT match '%s'\n\n", s, regex);
			}
			System.out.printf("Enter string --> ");
		}
	}

}
