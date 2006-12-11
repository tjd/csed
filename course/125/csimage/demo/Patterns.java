/*
 * Created on Jan 11, 2005 Class to demonstrate drawing simple patterns using
 * the matrix to date - stripes and checkers
 */
package csimage.demo;

import java.awt.Color;

import csimage.UberImage;

/**
 * @author John Edgar
 */
public class Patterns {

	public static void main(String[] args) {
		// TheMatrix m1 = new TheMatrix(50, 50, Color.RED, 10.0d, "Stripes");
		// striped(m1, Color.BLUE, Color.PINK, 21, 16);
		// //checkered(m1, Color.BLUE, Color.PINK, 4, 12);
		// m1.show();
	}

	/*
	 * Draws stripes of the indicated colors. The first stripe starts at the
	 * point specified by xStart, yStart. Thereafter each stripe commences a the
	 * start of each row.
	 */
	public static void striped(UberImage m, Color c1, Color c2, int xStart,
			int yStart) {
		// For each column
		for (int y = yStart; y < m.getHeight(); ++y) {
			// Change each pixel along one row
			for (int x = xStart; x < m.getWidth(); ++x) {
				if (y % 2 == 0) {
					m.setColor(x, y, c1);
				} else {
					m.setColor(x, y, c2);
				}
			}
			xStart = 0; // starts next row at the left edge
		}
	}// striped

	/*
	 * Draws a checkered pattern of the indicated colors. The pattern starts at
	 * the point specified by xStart, yStart. Thereafter the pattern commences a
	 * the start of each row.
	 */
	public static void checkered(UberImage m, Color c1, Color c2, int xStart,
			int yStart) {
		// For each column
		for (int y = yStart; y < m.getHeight(); ++y) {
			// Change each pixel along one row
			for (int x = xStart; x < m.getWidth(); ++x) {
				// Determine which color to make a pixel
				if (y % 2 == 0 && x % 2 == 0 || y % 2 == 1 && x % 2 == 1) {
					m.setColor(x, y, c1);
				} else {
					m.setColor(x, y, c2);
				}
			}
			xStart = 0; // starts next row at the left edge
		}
	}// checkered
}