package csimage.tests;

import java.util.Arrays;

import javax.imageio.ImageIO;

import csimage.UberImage;
import csimage.show;

public class testUberImage {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		System.out.println(Arrays.toString(ImageIO.getWriterFormatNames()));

		String base = "csimage/pictures/";
		// String[] pics = { "junk1.jpg", "lake1.gif", "lake1.jpg",
		// "Raphael.jpg",
		// "image.jpg", "Mandelbrot.png", "Sunset.jpg", "sumo.jpg" };
		UberImage gif = UberImage.fromFile(base + "lake1.gif");
		UberImage jpg = UberImage.fromFile(base + "sumo.jpg");
		UberImage png = UberImage.fromFile(base + "Mandelbrot.png");
		UberImage url = UberImage
				.fromURL("http://photos22.flickr.com/24776786_188bef9073.jpg");

		show.inFrame(gif);
		show.inFrame(jpg);
		show.inFrame(png);
		show.inFrame(url);
	}

}
