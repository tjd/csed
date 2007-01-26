package imagetest;

import java.awt.Color;
import java.util.Random;

import util.EasyInput;
import csimage.UberImage;
import csimage.show;

public class Grayscale {

	public static void main(String[] args) {
		checkAssertions();
		UberImage image = UberImage.fromFile(EasyInput.chooseFile());
		UberImage copy1 = new UberImage(image);
		show.inFrame(image);
		makeBlackAndWhite(image, 0.5);
		bwRandomDither(copy1);
		show.inFrame(copy1);
//		makeGrayscale(image);
//		makeOdd(image);
		
	}

	public static void checkAssertions() {
		try {
			assert 1 == 2;
			System.out.println("ASSERTIONS NOT ENABLED!!!");
			throw new Error(); // crash on purpose
		} catch (AssertionError e) {
			// okay
		}
	}

	public static double brightness(Color c) {
		return (c.getRed() + c.getGreen() + c.getBlue()) / (3.0 * 255);
	}

	public static void makeBlackAndWhite(UberImage image, double threshold) {
		assert threshold >= 0 && threshold <= 1 : "threshold = " + threshold;

		for (int i = 0; i < image.getWidth(); ++i) {
			for (int j = 0; j < image.getHeight(); ++j) {
				Color c = image.getColor(i, j);
				if (brightness(c) < threshold) {
					image.setColor(i, j, Color.BLACK);
				} else {
					image.setColor(i, j, Color.WHITE);
				}
			}
		}
	}

	public static Random rnd = new Random();
	
	public static void bwRandomDither(UberImage image) {
		for (int i = 0; i < image.getWidth(); ++i) {
			for (int j = 0; j < image.getHeight(); ++j) {
				Color c = image.getColor(i, j);
				double r = rnd.nextDouble();
				if (brightness(c) < r) {
					image.setColor(i, j, Color.BLACK);
				} else {
					image.setColor(i, j, Color.WHITE);
				}
			}
		}
	}
	public static void makeGrayscale(UberImage image) {
		for (int i = 0; i < image.getWidth(); ++i) {
			for (int j = 0; j < image.getHeight(); ++j) {
				Color c = image.getColor(i, j);
				double B = brightness(c);
				int x = (int) (255 * B);
				image.setColor(i, j, new Color(x, x, x));
			}
		}
	}

	public static void makeOdd(UberImage image) {
		for (int i = 0; i < image.getWidth(); ++i) {
			for (int j = 0; j < image.getHeight(); ++j) {
				Color c = image.getColor(i, j);
				int r = c.getRed();
				int g = c.getGreen();
				int b = c.getBlue();
				image.setColor(i, j, new Color(b, r, g));
			}
		}
	}

}
