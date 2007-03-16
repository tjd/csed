package strings;
import java.util.Date;

// Define a valid identifier as follows:
//   - It is a string of 1 or more characters.
//   - The first character is lowercase or uppercase letter, or the underscore symbol "_".
//   - The rest of the characters are lowercase letters, uppercase letters, the underscore
//     symbol, or the digits from 0 to 9.
//
// For example, "Cat_2_for_1", "__", and "ZWT" are all valid identifiers. But "up-down",
// "1derful", and " x" are invalid.
//
// Write a boolean method called isIdentifier(String s) that returns true if s is valid
// identifier, and false otherwise. Make your method as efficient as possible.

public class StringProb {

	public static final String UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

	public static final String LOWER = UPPER.toLowerCase();

	public static final String DIGITS = "0123456789";

	public static final String PUNC = "_";

	public static final String FIRST_LEGAL = UPPER + LOWER + PUNC;

	public static final String ALL_LEGAL = FIRST_LEGAL + DIGITS;

	// algorithm 1: basic loops
	public static boolean isIdentifier1(String s) {
		if (s.length() == 0 || FIRST_LEGAL.indexOf(s.charAt(0)) == -1) {
			return false;
		} else {
			for (int i = 1; i < s.length(); ++i) {
				if (ALL_LEGAL.indexOf(s.charAt(i)) == -1) {
					return false;
				}
			}
			return true;
		}
	}

	// algorithm2: recursive
	public static boolean isIdentifier2(String s) {
		if (s.length() == 0 || FIRST_LEGAL.indexOf(s.charAt(0)) == -1) {
			return false;
		} else {
			return isIdentifier2_aux(s, 0);
		}
	}

	private static boolean isIdentifier2_aux(String s, int start) {
		if (start == s.length()) {
			return true;
		} else if (ALL_LEGAL.indexOf(s.charAt(start)) == -1) {
			return false;
		} else {
			return isIdentifier2_aux(s, start + 1);
		}
	}

	// 48 to 57
	public static boolean isDigit(char c) {
		return c >= 48 && c <= 57;
	}

	// 97 to 122
	public static boolean isLower(char c) {
		return c >= 97 && c <= 122;
	}

	// 97 to 122
	public static boolean isUpper(char c) {
		return c >= 65 && c <= 90;
	}

	public static boolean isPunc(char c) {
		return c == '_';
	}

	public static boolean isFirstLegal(char c) {
		return isUpper(c) || isLower(c) || isPunc(c);
	}

	public static boolean isLegal(char c) {
		return isFirstLegal(c) || isDigit(c);
	}

	// algorithm 3: ASCII value checking
	public static boolean isIdentifier3(String s) {
		if (s.length() == 0 || !isFirstLegal(s.charAt(0))) {
			return false;
		} else {
			for (int i = 1; i < s.length(); ++i) {
				if (!isLegal(s.charAt(i))) {
					return false;
				}
			}
			return true;
		}
	}

	public static void isIdentifier1Test() {
		assert isIdentifier1("a");
		assert isIdentifier1("A");
		assert isIdentifier1("_");
		assert isIdentifier1("_A");
		assert isIdentifier1("Cat_2_for_1");
		assert isIdentifier1("taxRate");

		assert !isIdentifier1("");
		assert !isIdentifier1(" a");
		assert !isIdentifier1("a ");
		assert !isIdentifier1("2people");
		assert !isIdentifier1("Can-can");
		System.out.println("All isIdentifier1 tests passed");
	}

	public static void isIdentifier2Test() {
		assert isIdentifier2("a");
		assert isIdentifier2("A");
		assert isIdentifier2("_");
		assert isIdentifier2("_A");
		assert isIdentifier2("Cat_2_for_1");
		assert isIdentifier2("taxRate");

		assert !isIdentifier2("");
		assert !isIdentifier2(" a");
		assert !isIdentifier2("a ");
		assert !isIdentifier2("2people");
		assert !isIdentifier2("Can-can");
		System.out.println("All isIdentifier2 tests passed");
	}

	public static void isIdentifier3Test() {
		assert isIdentifier3("a");
		assert isIdentifier3("A");
		assert isIdentifier3("_");
		assert isIdentifier3("_A");
		assert isIdentifier3("Cat_2_for_1");
		assert isIdentifier3("taxRate");

		assert !isIdentifier3("");
		assert !isIdentifier3(" a");
		assert !isIdentifier3("a ");
		assert !isIdentifier3("2people");
		assert !isIdentifier3("Can-can");
		System.out.println("All isIdentifier3 tests passed");
	}

	public static void fileTest1() {
		String[] bill = util.EasyInput.fileToString(
				"/home/toby/Desktop/qa/shaks12.txt").split(" ");
		System.out.println("Start: " + new Date());
		
		int count = 0;
		for (int rep = 1; rep <= 5; ++rep) {
			count = 0;
			for (String word : bill) {
				if (isIdentifier1(word)) {
					++count;
				}
			}
		}
		System.out.printf("%s identifiers counted\n", count);
		System.out.println("End: " + new Date());
	}

	public static void fileTest2() {
		String[] bill = util.EasyInput.fileToString(
				"/home/toby/Desktop/qa/shaks12.txt").split(" ");
		System.out.println("Start: " + new Date());
		
		int count = 0;
		for (int rep = 1; rep <= 5; ++rep) {
			count = 0;
			for (String word : bill) {
				if (isIdentifier2(word)) {
					++count;
				}
			}
		}
		System.out.printf("%s identifiers counted\n", count);
		System.out.println("End: " + new Date());
	}

	public static void fileTest3() {
		String[] bill = util.EasyInput.fileToString(
				"/home/toby/Desktop/qa/shaks12.txt").split(" ");
		System.out.println("Start: " + new Date());
		
		int count = 0;
		for (int rep = 1; rep <= 5; ++rep) {
			count = 0;
			for (String word : bill) {
				if (isIdentifier3(word)) {
					++count;
				}
			}
		}
		System.out.printf("%s identifiers counted\n", count);
		System.out.println("End: " + new Date());
		;
	}

	public static void main(String[] args) {
		util.Util.ensureAssertionsEnabled();
		isIdentifier1Test();
		isIdentifier2Test();
		isIdentifier3Test();
		fileTest1();
	}

}
