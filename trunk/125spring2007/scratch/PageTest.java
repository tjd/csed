package scratch;

import util.EasyInput;

public class PageTest {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		String page = EasyInput.getWebPage("http://www.sfu.ca");
		System.out.println(page);
	}

}
