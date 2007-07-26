/**
 * Demonstrates random dithering.
 */
package csimage.demo.imageprocessing;

import java.util.Random;

import csimage.UberColor;
import csimage.UberImage;
import csimage.UberPixelMapper;
import csimage.show;

public class RandomDither {

	public static void main(String[] args) {
		final String fname = "csimage/pictures/sumo.jpg";
		final Random rnd = new Random();
		UberImage original = UberImage.fromFile(fname);
		UberImage m = UberImage.fromImage(original);
		show.inFrame(original);

		m.mapPixel(new UberPixelMapper() {
			public UberColor trans(int x, int y, UberColor c) {
				int lum = (int) c.luminance();
				int r = rnd.nextInt(40) - 20;
				int m2_val = lum + r;
				if (m2_val < 100) {
					return UberColor.BLACK;
				} else {
					return UberColor.WHITE;
				}
			}
		});

		show.inFrame(m);
		System.out.printf("done");
	}
}