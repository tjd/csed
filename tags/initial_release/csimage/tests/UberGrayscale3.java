package csimage.tests;

/**
 * Demonstrates how to use the applyAllPixels method by converting a color image
 * to grayscale.
 */

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Date;

import javax.imageio.ImageIO;

import csimage.UberColor;
import csimage.UberImage;
import csimage.show;

public class UberGrayscale3 {

	final static String fname = "csimage/pictures/Raphael.jpg";

	public static void test1() {
		UberImage img = UberImage.fromFile(fname);
		System.out.printf("# of pixels = %s\n", img.getWidth()
				* img.getHeight());
		int rgb = img.getRGB(50, 50);
		byte b = (byte) rgb;
		byte g = (byte) (rgb >> 8);
		byte r = (byte) (rgb >> 16);
		byte a = (byte) (rgb >> 24);

		int lum = (int) (255 * (r + g + b) / (3.0 * 255));
		int gscale = (lum << 24) + (lum << 16) + (lum << 8) + lum;
		System.out.printf("lum = %s, gscale = %s\n", lum, gscale);

		UberColor c = img.getColor(50, 50);

		assert c.getRGB() == rgb;
		assert ((byte) c.getAlpha()) == a : String.format(
				"c.getAlpha() == %s, a = %s", c.getAlpha(), a);
		assert ((byte) c.getRed()) == r;
		assert ((byte) c.getGreen()) == g;
		assert ((byte) c.getBlue()) == b;

		System.out.printf("rgb = %s, r = %s, g = %s, b = %s, a = %s", rgb, r,
				g, b, a);
		System.out.printf("\nx = %s", (byte) 258);
	}

	public static void test2() {
		UberImage img = UberImage.fromFile(fname);
		show.inFrame(img);
		final int width = img.getWidth();
		final int height = img.getHeight();
		for (int i = 0; i < width; i++) {
			for (int j = 0; j < height; j++) {
				int rgb = img.getRGB(i, j);
				int b = (rgb & 0x000000FF);
				int g = (rgb & 0x0000FF00) >> 8;
				int r = (rgb & 0x00FF0000) >> 16;
				// System.out.printf("%x = rgb\n%x = r\n%x = g\n%x = b\n", rgb,
				// r,
				// g, b);
				// Color c = img.getColor(i, j);
				// assert b == c.getBlue();
				// assert r == c.getRed();
				// assert g == c.getGreen();
				// int a = (rgb & 0xFF000000) >> 24;
				// assert 0 <= b && b <= 255;
				// assert 0 <= g && g <= 255;
				// assert 0 <= r && r <= 255;

				int lum = (int) ((r + g + b) / 3.0);
				// assert 0 <= lum && lum <= 255;
				int alpha = rgb & 0xFF000000;
				int red = lum << 16;
				int green = lum << 8;
				int blue = lum;
				int newARGB = alpha | red | green | blue;
				// System.out
				// .printf(
				// "%x = lum\n%x = alpha\n%x = red\n%x = green\n%x = blue\n%x =
				// newARGB\n",
				// lum, alpha, red, green, blue, newARGB);

				img.setRGB(i, j, newARGB);
				// Color c2 = img.getColor(i,j);
				// assert c2.getAlpha() == (alpha >>> 24) : String.format(
				// "c.getAlpha() = %x, alpha = %x", c2.getAlpha(), alpha >>>
				// 24);
				// assert c2.getRed() == red;
				// assert c2.getGreen() == green;
				// assert c2.getBlue() == blue;
			}
		}

		System.out.println("Pixels proccessed ... showing result ...");
		show.inFrame(img);
	}

	public static void test3() {
		UberImage img = UberImage.fromFile(fname);
		// show.asFrame(img);
		final int width = img.getWidth();
		final int height = img.getHeight();
		int[] dummy = null;
		int[] data = img.getData().getPixels(0, 0, width, height, dummy);
		System.out.printf("%s x %s  = %s ==? %s\n", width, height, width
				* height, data.length);
		for (int i = 0; i < 10; ++i) {
			System.out.printf("data[%s] = %x\n", i, data[i]);
		}
		System.out.printf("(0, 0) = %x", img.getRGB(0, 0));
	}

	public static void dprint(String s) {
		System.out.println(s + " " + (new Date()));
	}

	// Creating buffered image: Fri Jul 22 14:30:05 PDT 2005
	// .. done Fri Jul 22 14:30:07 PDT 2005
	// Starting grayscale conversion: Fri Jul 22 14:30:07 PDT 2005
	// ... done Fri Jul 22 14:30:30 PDT 2005
	public static void test4() {
		try {
			dprint("Creating buffered image:");
			BufferedImage img = ImageIO.read(new File(fname));
			dprint(".. done");
			final int width = img.getWidth();
			final int height = img.getHeight();
			dprint("Starting grayscale conversion:");
			for (int i = 0; i < width; i++) {
				for (int j = 0; j < height; j++) {
					int rgb = img.getRGB(i, j);
					int b = (rgb & 0x000000FF);
					int g = (rgb & 0x0000FF00) >> 8;
					int r = (rgb & 0x00FF0000) >> 16;

					int lum = (int) ((r + g + b) / 3.0);
					int alpha = rgb & 0xFF000000;
					int red = lum << 16;
					int green = lum << 8;
					int blue = lum;
					int newARGB = alpha | red | green | blue;

					img.setRGB(i, j, newARGB);
				}
			}
			dprint("... done");
			show.inFrame(img);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public static void main(String[] args) {
		// test1();
		// test2();
		// test3();
		test4();
	}
}