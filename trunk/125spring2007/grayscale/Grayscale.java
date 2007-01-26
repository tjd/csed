package grayscale;

import java.awt.Color;
import java.util.Random;

import util.EasyInput;
import csimage.UberImage;
import csimage.show;

public class Grayscale {

	public static void main(String[] args) {
		UberImage image = UberImage.fromFile(EasyInput.chooseFile());
		show.inFrame(image);
		UberImage copy1 = new UberImage(image);
		UberImage copy2 = new UberImage(image);
		System.out.print("Calling makeBlackAndWhite ... ");
		makeBlackAndWhite(copy1);
		System.out.println("done");
		show.inFrame(copy1);
		System.out.print("Calling bwRandomDither ...");
		bwRandomDither(copy2);
		System.out.println("done");
		show.inFrame(copy2);
	}

	public static void makeBlackAndWhite(UberImage image) {
		for (int i = 0; i < image.getWidth(); ++i) {
			for (int j = 0; j < image.getHeight(); ++j) {
				Color c = image.getColor(i, j);
				if (brightness(c) < 0.5) {
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

	public static double brightness(Color c) {
		return (c.getRed() + c.getGreen() + c.getBlue()) / (3.0 * 255);
	}

}
