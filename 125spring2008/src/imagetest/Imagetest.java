package imagetest;

import java.awt.Color;

import csimage.UberImage;
import csimage.show;

public class Imagetest {

	public static void main(String[] args) {
		String fname = "C:\\Documents and Settings\\tjd\\Desktop\\125\\baby.jpg";
		UberImage img1 = UberImage.fromFile(fname);
		UberImage img2 = UberImage.fromImage(img1);

		makeWeird(img2);
		show.inFrame(img2);
		show.inFrame(img1);
	}

	public static void makeBlackAndWhite(UberImage img) {
		for (int i = 0; i < img.getWidth(); ++i) {
			for (int j = 0; j < img.getHeight(); ++j) {
				Color c = img.getColor(i, j);
				int brightness = c.getRed() + c.getGreen() + c.getBlue();
				final double THRESHOLD = (255 * 3) * 0.5;
				if (brightness < THRESHOLD) {
					img.setColor(i, j, Color.BLACK);
				} else {
					img.setColor(i, j, Color.WHITE);
				}
			}
		}
	}

	public static void makeGrayscale(UberImage img) {
		for (int i = 0; i < img.getWidth(); ++i) {
			for (int j = 0; j < img.getHeight(); ++j) {
				Color c = img.getColor(i, j);
				int brightness = (int) (255 * (c.getRed() + c.getGreen() + c
						.getBlue()) / (3.0 * 255));
				img.setColor(i, j,
						new Color(brightness, brightness, brightness));
			}
		}
	}

	public static void makeWeird(UberImage img) {
		for (int i = 0; i < img.getWidth(); ++i) {
			for (int j = 0; j < img.getHeight(); ++j) {
				Color c = img.getColor(i, j);
				int brightness = c.getRed() + c.getGreen() + c.getBlue();
				if (brightness < (255*3)/3) {
					img.setColor(i, j, Color.BLUE);
				} else if (brightness < 2*(255*3)/3){
					img.setColor(i, j, Color.PINK);
				} else {
					img.setColor(i, j, Color.YELLOW);
				}
			}
		}
	}

}
