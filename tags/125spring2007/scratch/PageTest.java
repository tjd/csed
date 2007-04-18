package scratch;

import javax.swing.JOptionPane;

import util.EasyInput;

public class PageTest {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		String url = JOptionPane.showInputDialog("Enter a URL:");
		String page = EasyInput.getWebPage(url);
		System.out.println(page);
	}

}
