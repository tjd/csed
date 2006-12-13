package statemachines;

public class Machine {

	public static boolean M1(String s) {
		if (s.equals("")) {
			return false;
		} else {
			int i = 0;
			
			// check for 'a's
			while (s.charAt(i) == 'a') {
				++i;
				if (i == s.length()) {
					return false;
				}
			}

			// check for 'b's
			for (; i < s.length(); ++i) {
				if (s.charAt(i) != 'b') {
					return false;
				}
			}
			return true;
		}
	}

	enum TwoState {
		S1, S2, error
	}

	public static boolean M2(String s) {
		TwoState state = TwoState.S1;
		for (int i = 0; i < s.length(); ++i) {
			char ch = s.charAt(i);
			if (state == TwoState.S1) {
				if (ch == 'a') {
					state = TwoState.S1;
				} else if (ch == 'b') {
					state = TwoState.S2;
				}
			} else if (state == TwoState.S2) {
				if (ch == 'a') {
					state = TwoState.error;
				} else if (ch == 'b') {
					state = TwoState.S2;
				}
			} else if (state == TwoState.error) {
				if (ch == 'a') {
					state = TwoState.error;
				} else if (ch == 'b') {
					state = TwoState.error;
				}
			}
		}
		return state == TwoState.S2;
	}

	public boolean A(String s) {
		return true;
	}

	enum ThreeState {
		S1, S2, S3, error
	}

	public static boolean B(String s) {
		ThreeState state = ThreeState.S1;
		for (int i = 0; i < s.length(); ++i) {
			char ch = s.charAt(i);
			if (state == ThreeState.S1) {
				if (ch == 'a') {
					state = ThreeState.S2;
				} else if (ch == 'b') {
					state = ThreeState.error;
				}
			} else if (state == ThreeState.S2) {
				if (ch == 'a') {
					state = ThreeState.S2;
				} else if (ch == 'b') {
					state = ThreeState.S3;
				}
			} else if (state == ThreeState.S3) {
				if (ch == 'a') {
					state = ThreeState.error;
				} else if (ch == 'b') {
					state = ThreeState.S3;
				}
			} else if (state == ThreeState.error) {
				if (ch == 'a') {
					state = ThreeState.error;
				} else if (ch == 'b') {
					state = ThreeState.error;
				}
			}
		}
		return state == ThreeState.S3;
	}

	public static void testB() {
		assert B("ab");
		assert B("aaaab");
		assert B("abbbbbb");
		assert B("aaaaabbbb");
		
		assert !B("");
		assert !B("a");
		assert !B("b");
		assert !B("aba");
		assert !B("bab");
		assert !B("bbaaa");
		
		System.out.printf("all tests passed\n");
	}
	
	public static void testM2() {
		assert M2("b");
		assert M2("bb");
		assert M2("aaab");
		assert M2("abbb");

		assert !M2("");
		assert !M2("a");
		assert !M2("aa");
		assert !M2("aba");
		assert !M2("babb");
		assert !M2("abbba");

		System.out.printf("all tests passed\n");
	}

	public static void testM1() {
		assert M1("b");
		assert M1("bb");
		assert M1("aaab");
		assert M1("abbb");

		assert !M1("");
		assert !M1("a");
		assert !M1("aa");
		assert !M1("aba");
		assert !M1("babb");
		assert !M1("abbba");

		System.out.printf("all tests passed\n");
	}

	public static void main(String[] args) {
		testM1();
		testM2();
		testB();
	}

}
