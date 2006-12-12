package unittest;

public class ReplaceLessThan {

	public static String replaceLessThan(String s) {
		String result = "";
		for (int i = 0; i < s.length(); ++i) {
			char ch = s.charAt(i);
			if (ch == '<') {
				result = result + "&lt;";
			} else {
				result = result + ch;
			}
		}
		return result;
	}

	public static void replaceLessThanTest() {
//		assert 1 == 2;
		assert replaceLessThan("").equals("");
		assert replaceLessThan("a").equals("a");
		assert replaceLessThan("<").equals("&lt;");
		assert replaceLessThan("<<").equals("&lt;&lt;");
		assert replaceLessThan("I like cheese.").equals("I like cheese.");
		assert replaceLessThan("If x < y or a < b").equals(
				"If x &lt; y or a &lt; b");
		System.out.print("all replaceLessThan tests passed");
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		String s = "If a < b, then a < c.";
		System.out.printf("replaceLessThan(\"%s\") = \"%s\"\n", s,
				replaceLessThan(s));
		replaceLessThanTest();
	}

}
