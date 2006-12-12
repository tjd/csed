package csimage.demo;

import csimage.UberColor;
import csimage.UberImage;
import csimage.show;

/*
 * Problem: Create an 8-by-8 checkboard pattern of alternating green and white
 * squares.
 */

public class Checkers {

	public static void main(String[] args) {
		final int SIDE = 50;
		final int WIDTH = 8 * SIDE;
		final int HEIGHT = 8 * SIDE;
		UberImage m = UberImage.blankImage(WIDTH, HEIGHT);

		boolean firstColor = true;
		for (int i = 0; i < WIDTH; i += SIDE) {
			firstColor = !firstColor;
			for (int j = 0; j < HEIGHT; j += SIDE) {
				if (firstColor) {
					m.fillSquare(i, j, SIDE, UberColor.GREEN);
				} else {
					m.fillSquare(i, j, SIDE, UberColor.WHITE);
				}
				firstColor = !firstColor;
			}
		}
		show.inFrame(m);
	}
}