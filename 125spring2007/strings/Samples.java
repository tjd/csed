package strings;

public class Samples {


	public static String reverse1(String s) {
		String result = "";
		for(int i = s.length() - 1; i >= 0; --i) {
			result += s.charAt(i);
		}
		return result;
	}
	
	public static String reverse2(String s) {
		StringBuilder work = new StringBuilder();
		for(int i = s.length() - 1; i >= 0; --i) {
			work.append(s.charAt(i));
		}
		return work.toString();
	}
	
	public static void main(String[] args) {
		System.out.println(reverse1("cattle"));
		System.out.println(reverse1("balloon"));
		System.out.println(reverse2("cattle"));
		System.out.println(reverse2("balloon"));
		double gpa = Double.parseDouble("3.25");
	}

}
