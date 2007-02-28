package csimage.demo.starter;

import java.awt.Color;

import csimage.UberImage;

public class Effects {

	public static void makeBlackAndWhite(UberImage pic) {
		final int THRESHOLD = (255 + 255 + 255) / 2;
		for (int i = 0; i < pic.getWidth(); ++i) {
			for (int j = 0; j < pic.getHeight(); ++j) {
				Color c = pic.getColor(i, j);
				if (c.getRed() + c.getGreen() + c.getBlue() <= THRESHOLD) {
					pic.setColor(i, j, Color.WHITE);
				} else {
					pic.setColor(i, j, Color.BLACK);
				}
			}
		}
	}
}