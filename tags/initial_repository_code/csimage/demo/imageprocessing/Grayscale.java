package csimage.demo.imageprocessing;

/**
 * Demonstrates how to use the applyAllPixels method by converting a color image
 * to grayscale.
 */

import csimage.UberColor;
import csimage.UberImage;
import csimage.UberPixelMapper;
import csimage.show;

public class Grayscale {

	final static String fname = "csimage/pictures/sumo.jpg";

	public static void main(String[] args) {
		UberImage original = UberImage.fromFile(fname);
		UberImage m = UberImage.fromImage(original);
		show.inFrame(original);

		m.mapPixel(new UberPixelMapper() {
			public UberColor trans(int x, int y, UberColor c) {
				int lum = (int) c.luminance();
				return new UberColor(lum, lum, lum);
			}
		});
		show.inFrame(m);
	}
}