package webpage;

import util.EasyInput;

public class SimpleWebBrowser {

	public static void main(String[] args) {
		String page = EasyInput.getWebPage("http://www.yahoo.com");
		System.out.print(page);
	}

}
