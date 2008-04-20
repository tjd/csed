package csimage.demo.wolfram;

/**
 * @author Karl Schmidt
 *  
 */

import java.awt.Color;

import csimage.UberImage;
import csimage.show;

public class WolframAutomata2 {
	public static void main(String[] args) {
		test1();
		test2();
		test3();
	}

	public static void test1() {
		drawWolfram((byte) 30);
	}

	public static void test2() {
		drawWolfram((byte) 90);
	}

	public static void test3() {
		drawWolfram((byte) 110);
	}

	/*
	 * This is hard-coded to create a matrix of a hard-coded size Although using
	 * strings to evaulate the patterns isn't super-efficient, I was going after
	 * an easier-read and maintain approach.
	 */
	public static void drawWolfram(byte rule) {
		UberImage theImage = UberImage.blankImage(800, 800, Color.WHITE);

		theImage.setColor(400, 0, Color.BLACK);
		for (int y = 0; y < theImage.getHeight() - 1; y++) {
			for (int x = 1; x < theImage.getWidth() - 1; x++) {
				String current = checkSection(x, y, theImage);

				if (current.equals("BBB")) {
					if ((rule & (byte) 128) == 128) {
						theImage.setColor(x, y + 1, Color.BLACK);
					} else {
						theImage.setColor(x, y + 1, Color.WHITE);
					}
				} else if (current.equals("BBW")) {
					if ((rule & (byte) 64) == 64) {
						theImage.setColor(x, y + 1, Color.BLACK);
					} else {
						theImage.setColor(x, y + 1, Color.WHITE);
					}
				} else if (current.equals("BWB")) {
					if ((rule & (byte) 32) == 32) {
						theImage.setColor(x, y + 1, Color.BLACK);
					} else {
						theImage.setColor(x, y + 1, Color.WHITE);
					}
				} else if (current.equals("BWW")) {
					if ((rule & (byte) 16) == 16) {
						theImage.setColor(x, y + 1, Color.BLACK);
					} else {
						theImage.setColor(x, y + 1, Color.WHITE);
					}
				} else if (current.equals("WBB")) {
					if ((rule & (byte) 8) == 8) {
						theImage.setColor(x, y + 1, Color.BLACK);
					} else {
						theImage.setColor(x, y + 1, Color.WHITE);
					}
				} else if (current.equals("WBW")) {
					if ((rule & (byte) 4) == 4) {
						theImage.setColor(x, y + 1, Color.BLACK);
					} else {
						theImage.setColor(x, y + 1, Color.WHITE);
					}
				} else if (current.equals("WWB")) {
					if ((rule & (byte) 2) == 2) {
						theImage.setColor(x, y + 1, Color.BLACK);
					} else {
						theImage.setColor(x, y + 1, Color.WHITE);
					}
				} else if (current.equals("WWW")) {
					if ((rule & (byte) 1) == 1) {
						theImage.setColor(x, y + 1, Color.BLACK);
					} else {
						theImage.setColor(x, y + 1, Color.WHITE);
					}
				}
			}
		}
		show.inFrame(theImage);
	}

	private static String checkSection(int x, int y, UberImage image) {
		String result = "";

		if (x == 0) {
			result += "W";
			if (image.getColor(x, y).equals(Color.BLACK)) {
				result += "B";
			} else {
				result += "W";
			}
			if (image.getColor(x + 1, y).equals(Color.BLACK)) {
				result += "B";
			} else {
				result += "W";
			}
		} else if (x == image.getWidth() - 1) {
			if (image.getColor(x - 1, y).equals(Color.BLACK)) {
				result += "B";
			} else {
				result += "W";
			}
			if (image.getColor(x, y).equals(Color.BLACK)) {
				result += "B";
			} else {
				result += "W";
			}
			result += "W";
		} else {
			if (image.getColor(x - 1, y).equals(Color.BLACK)) {
				result += "B";
			} else {
				result += "W";
			}
			if (image.getColor(x, y).equals(Color.BLACK)) {
				result += "B";
			} else {
				result += "W";
			}
			if (image.getColor(x + 1, y).equals(Color.BLACK)) {
				result += "B";
			} else {
				result += "W";
			}
		}

		return result;
	}

}
