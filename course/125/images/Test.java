package images;

import java.awt.Color;
import java.util.Random;

import csimage.UberImage;
import csimage.show;

public class Test {

	public final static String IMAGE_DIR = "/Users/toby/Desktop/spring 2006/125/images/";

	public final static Random rnd = new Random();

	public static void main(String[] args) {
		UberImage newton = UberImage.fromFile(IMAGE_DIR + "newton.jpg");
//		UberImage drnebulon = UberImage.fromFile(IMAGE_DIR + "drnebulon.jpg");

		
		show.inFrame(newton);
//		show.inFrame(drnebulon);
		int width = newton.getWidth();
		int height = newton.getHeight();

		UberImage blend = UberImage.blankImage(width, height);

//		double p = 0.5;
//		double q = 1 - p;

		for (int i = 0; i < width; ++i) {
			for (int j = 0; j < height; ++j) {
				Color a = newton.getColor(i, j);
				int r = rnd.nextInt(3);
				if (r == 0) {
					blend.setColor(i, j,
							new Color(0, a.getGreen(), a.getBlue()));
				} else if (r == 1) {
					blend.setColor(i, j, new Color(a.getRed(), 0, a.getBlue()));
				} else {
					blend
							.setColor(i, j, new Color(a.getRed(), a.getGreen(),
									0));
				}
			}
		}

		show.inFrame(blend);
		
	}

}
