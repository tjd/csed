package recursion;

public class Acronym {

	public static String expand(String acro, String exp, int n) {
		if (n == 1) {
			return exp;
		} else {
			return exp.replace(acro, expand(acro, exp, n - 1));
		}
	}

	public static void main(String[] args) {
		for (int i = 1; i <= 10; ++i) {
			System.out.println(expand("GNU", "GNU's Not Unix", i));
		}
		for (int i = 1; i <= 10; ++i) {
			System.out.println(expand("TTP", "The TTP Project", i));
		}
		
		for (int i = 1; i <= 10; ++i) {
			System.out.println(expand("TOTO", "TOTO Own's TOTO's Options", i));
		}
	}

}
