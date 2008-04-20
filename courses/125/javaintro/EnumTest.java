package javaintro;

import java.util.Scanner;

/*
 * 
 * EnumTest.java
 * 
 * Demonstrates basic use of enumerated types. Enums are new in Java 1.5.
 * 
 */

public class EnumTest {

	enum Day {
		monday, tuesday, wednesday, thursday, friday, saturday, sunday
	}

	public static void main(String[] args) {
		Day day1 = Day.monday;
		Day day2 = Day.thursday;
		Day day3 = Day.sunday;

		System.out.printf("day1 = %s, ordinal = %d, name = \"%s\"\n", day1,
				day1.ordinal(), day1.name());
		System.out.printf("day2 = %s, ordinal = %d, name = \"%s\"\n", day2,
				day2.ordinal(), day2.name());
		System.out.printf("day3 = %s, ordinal = %d, name = \"%s\"\n", day3,
				day3.ordinal(), day3.name());

		Scanner in = new Scanner(System.in);
		System.out.print("\nPlease enter a weekday name: ");
		String s = in.next().trim().toLowerCase();
		Day inDay = Day.valueOf(s);
		System.out.printf("inDay = %s", inDay);
	}

}
