package grayscale;

import java.awt.Color;

import util.EasyInput;
import csimage.UberImage;
import csimage.show;

public class Grayscale {

	public static void main(String[] args) {
		UberImage image = UberImage.fromFile(EasyInput.chooseFile());
		makeBlackAndWhite(image);
		show.inFrame(image);
	}

	public static void makeBlackAndWhite(UberImage image) {
		for (int i = 0; i < image.getWidth(); ++i) {
			for (int j = 0; j < image.getHeight(); ++j) {
				Color c = image.getColor(i, j);
				int brightness = c.getRed() + c.getGreen() + c.getBlue();
				if (brightness < 3 * 255 / 2) {
					image.setColor(i, j, Color.WHITE);
				} else {
					image.setColor(i, j, Color.BLACK);
				}
			}
		}
	}

}
