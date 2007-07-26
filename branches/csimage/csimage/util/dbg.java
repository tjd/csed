package csimage.util;

public class dbg {
	public static boolean DEBUG = true;

	public static void say(String s) {
		if (DEBUG) {
			System.out.print(s);
		} // if
	}

	public static void sayln(String s) {
		say(s + "\n");
	}

	public static String quote(String s) {
		return "\"" + s + "\"";
	}

	public static String quoteln(String s) {
		return quote(s) + "\n";
	}

	public static String pair(int a, int b) {
		return pair("" + a, "" + b);
	}

	public static String pair(String a, String b) {
		return pair("", a, b);
	}

	public static String pairln(String a, String b) {
		return pair(a, b) + "\n";
	}

	public static String pair(String msg, String a, String b) {
		return msg + "(" + a + ", " + b + ")";
	}

	public static String pairln(String msg, String a, String b) {
		return pair(msg, a, b) + "\n";
	}
}
