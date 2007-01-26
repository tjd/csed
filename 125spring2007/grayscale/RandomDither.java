package grayscale;

import java.awt.Color;
import java.util.Random;

import util.EasyInput;
import csimage.UberImage;
import csimage.show;

public class RandomDither {

	public static void main(String[] args) {
		UberImage image = UberImage.fromFile(EasyInput.chooseFile());
		bwRandomDither(image);
		show.inFrame(image);
	}

	public static Random rnd = new Random();
	
	public static void bwRandomDither(UberImage image) {
		for (int i = 0; i < image.getWidth(); ++i) {
			for (int j = 0; j < image.getHeight(); ++j) {
				Color c = image.getColor(i, j);
				int r = rnd.nextInt(256);
				if (brightness(c) < r) {
					image.setColor(i, j, Color.BLACK);
				} else {
					image.setColor(i, j, Color.WHITE);
				}
			}
		}
	}

	public static int brightness(Color c) {
		return c.getRed() + c.getGreen() + c.getBlue();
	}

}
