package recursion;

public class RecursiveAcronym {

	public static void main(String[] args) {
//		String acro = "GNU";
//		String exp = "GNU's Not Unix";
		String acro = "MOMS";
		String exp = "MOMS Offering MOMS Support";

		for (int i = 0; i < 10; ++i) {
			String expansion = expand(acro, exp, i);
			System.out.printf("%s: %s\n(%s characters long)\n\n", i, expansion, expansion.length());
		}

	}

	// expands the given recursive acronym n times 
	public static String expand(String acro, String expansion, int n) {
		if (n == 0) {
			return acro;
		} else if (n == 1) {
			return expansion;
		} else { // n >= 2
			String result = expansion.replaceAll(acro, expansion);
			for (int i = 1; i < n; ++i) {
				result = result.replaceAll(acro, expansion);
			}
			return result;
		}
	}

}
