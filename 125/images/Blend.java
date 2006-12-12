package images;

import java.awt.Color;

import csimage.UberImage;
import csimage.show;

public class Blend {

	public final static String IMAGE_DIR = "/Users/toby/Desktop/spring 2006/125/images/";

	public static void main(String[] args) {
		UberImage newton = UberImage.fromFile(IMAGE_DIR + "newton.jpg");
		UberImage drnebulon = UberImage.fromFile(IMAGE_DIR + "drnebulon.jpg");
		show.inFrame(newton);
		show.inFrame(drnebulon);

		assert newton.getWidth() == drnebulon.getWidth();
		assert newton.getHeight() == drnebulon.getHeight();

		final int width = newton.getWidth();
		final int height = newton.getHeight();

		UberImage blend = UberImage.blankImage(width, height);

		final double p = 0.8;
		final double q = 1 - p;

		for (int i = 0; i < width; ++i) {
			for (int j = 0; j < height; ++j) {
				Color a = newton.getColor(i, j);
				Color b = drnebulon.getColor(i, j);
				int ir = (int) (p * a.getRed() + q * b.getRed());
				int ig = (int) (p * a.getGreen() + q * b.getGreen());
				int ib = (int) (p * a.getBlue() + q * b.getBlue());
				Color c = new Color(ir, ig, ib);
				blend.setColor(i, j, c);
			}
		}

		show.inFrame(blend);
		blend.saveAsJPEG(IMAGE_DIR + "blend" + p + ".jpg");
	}

}
