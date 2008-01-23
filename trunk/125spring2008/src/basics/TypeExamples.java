package basics;

import java.util.HashMap;

public class TypeExamples {

	public static void main(String[] args) {
		int a = 4;
		int b = 8 + 3;
		int c = (1 + 2) * (40 / 10);
		int d = 2 * a + b;

		HashMap<Integer, String> map = new HashMap<Integer, String>();

		String s = "*";
		String rect = repeat(s, 10) + "\n" + repeat(s, 10);
		System.out.printf("%s\n\n", rect);
		System.out.printf("%s", rectangle("@", 10, 5));
		
		System.out.printf("%s\n", trim("   abc def  "));
		System.out.printf("%s\n", trim("abc def", "."));
	}

	public static String repeat(String s, int times) // header
	{
		String result = "";
		for (int i = 0; i < times; ++i) {
			result += s;
		}
		return result;
	}

	public static String rectangle(String s, int columns, int rows) {
		return repeat(repeat(s, columns) + "\n", rows);
	}

	public static String trim(String s, char c) {
		int begin = 0;
		while (s.charAt(begin) == c) {
			++begin;
		}

		int end = s.length() - 1;
		while (s.charAt(end) == c) {
			--end;
		}
		return s.substring(begin, end + 1);
	}
	
	public static String trim(String s) {
		return trim(s, ' ');
	}
	
	public static String trim(String s, String c) {
		return trim(s, c.charAt(0));
	}

}
