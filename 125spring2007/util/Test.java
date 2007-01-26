package util;

public class Test {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
//		Color c = EasyInput.chooseColor();
//		System.out.println(c.toString());
		String[] files = EasyInput.listdir();
		for(String s : files) {
			System.out.println(s);
		}
		String yahoo = EasyInput.getWebPage("http://www.yahoo.com");
		System.out.println(yahoo);
	}

}
