package statemachines;

public class StateMachine {

	public static void main(String[] args) {
		test1();
	}

	private static void test1() {
		assert isEmail("billg@microsoft.com");
		assert isEmail("1@2.3");
		assert isEmail("tjd@sfu.ca");
		assert isEmail("tjd23@1sfu.ca33");
		assert !isEmail("tjd@sfu.");
		assert !isEmail("tjd@.ca");
		assert !isEmail("@sfu.");
		assert !isEmail("@.");
		assert !isEmail("tjd.sfu@ca");
		assert !isEmail("tjd@sfu.ca.com");
		System.out.println("All tests passed.");
	}

	final public static String ALPHA_NUMERIC = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";

	public static boolean isEmail(String a) {
		int S = 1;
		for (int i = 0; i < a.length(); i++) {
			char c = a.charAt(i);
			if (S == 1) {
				if (ALPHA_NUMERIC.indexOf(c) != -1) {
					S = 2;
				} else {
					return false;
				}
			} else if (S == 2) {
				if (ALPHA_NUMERIC.indexOf(c) != -1) {
					S = 2; // no change
				} else if (c == '@') {
					S = 3;
				} else {
					return false;
				}
			} else if (S == 3) {
				if (ALPHA_NUMERIC.indexOf(c) != -1) {
					S = 3;
				} else {
					return false;
				}
			} else if (S == 4) {
				if (ALPHA_NUMERIC.indexOf(c) != -1) {
					S = 4; // no change
				} else if (c == '.') {
					S = 5;
				} else {
					return false;
				}
			} else if (S == 5) {
				if (ALPHA_NUMERIC.indexOf(c) != -1) {
					S = 6;
				} else {
					return false;
				}
			} else if (S == 6) {
				if (ALPHA_NUMERIC.indexOf(c) != -1) {
					S = 6;
				} else {
					return false;
				}
			} else {
				throw new Error("Oh no! Something terrible has happened! S = "
						+ S + ", but it should in the range 1 - 6.");
			}
		}

		if (S == 6) {
			return true;
		} else {
			return false;
		}
	}

}