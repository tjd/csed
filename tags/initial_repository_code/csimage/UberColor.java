package csimage;

import java.awt.Color;
import java.awt.color.ColorSpace;

import csimage.util.dbg;

public class UberColor extends Color {

	public static UberColor WHITE = new UberColor(Color.WHITE);

	public static UberColor BLACK = new UberColor(Color.BLACK);

	public static UberColor BLUE = new UberColor(Color.BLUE);

	public static UberColor CYAN = new UberColor(Color.CYAN);

	public static UberColor DARK_GRAY = new UberColor(Color.DARK_GRAY);

	public static UberColor LIGHT_GRAY = new UberColor(Color.LIGHT_GRAY);

	public static UberColor GRAY = new UberColor(Color.GRAY);

	public static UberColor GREEN = new UberColor(Color.GREEN);

	public static UberColor MAGENTA = new UberColor(Color.MAGENTA);

	public static UberColor ORANGE = new UberColor(Color.ORANGE);

	public static UberColor PINK = new UberColor(Color.PINK);

	public static UberColor RED = new UberColor(Color.RED);

	public static UberColor YELLOW = new UberColor(Color.YELLOW);

	public UberColor(ColorSpace cspace, float[] components, float alpha) {
		super(cspace, components, alpha);
	}

	public UberColor(float r, float g, float b, float alpha) {
		super(r, g, b, alpha);
	}

	public UberColor(float r, float g, float b) {
		super(r, g, b);
	}

	public UberColor(int rgba, boolean hasAlpha) {
		super(rgba, hasAlpha);
	}

	public UberColor(int r, int g, int b, int alpha) {
		super(r, g, b, alpha);
	}

	public UberColor(int r, int g, int b) {
		super(r, g, b);
	}

	public UberColor(int rgb) {
		super(rgb);
	}

	/**
	 * Copy constructor.
	 * 
	 * @param c
	 *            the color to copy
	 */
	public UberColor(Color c) {
		this(c.getRed(), c.getGreen(), c.getBlue());
	}

	/**
	 * Converts an HTML-style color in the form #RRGGBB to an UberColor object.
	 * 
	 * @param s
	 *            an HTML hex color in the form "#RRGGBB"
	 * @return a new UberColor matching the color specified by s
	 */
	public static UberColor fromHTMLcolor(String s) {
		s = s.trim();
		assert s.charAt(0) == '#' : dbg.quote(s) + " must start with #";
		assert s.length() == 7 : dbg.quote(s) + " is not in #RRGGBB format";

		s = s.substring(1);
		String r = s.substring(0, 2);
		String g = s.substring(2, 4);
		String b = s.substring(4, 6);
		UberColor c = new UberColor(Integer.valueOf(r, 16), Integer.valueOf(g,
				16), Integer.valueOf(b, 16));
		return c;
	}

	/**
	 * Returns the "lightness" of the given RGB value. This definition of
	 * lightness is taken from <a href = "http://gimp-savvy.com/BOOK/">Grokking
	 * the Gimp </a>. It's an approximation of true lightness.
	 * 
	 * @return the lightness of this color
	 */
	public double lightness() {
		final int R = getRed();
		final int G = getGreen();
		final int B = getBlue();
		return (Math.max(Math.max(R, G), B) + Math.min(Math.min(R, G), B)) / 2;
	}

	/**
	 * Returns the luminance of the given pixel Standard definition of
	 * luminance. The coefficients are standard and chosen with regard to the
	 * human eye's sensitivity to red, green, and blue, and the phosphors in a
	 * typical monitor.
	 * 
	 * @see <a
	 *      href="http://www.poynton.com/notes/colour_and_gamma/ColorFAQ.html#RTFToC1">Charles
	 *      Poynton's Color FAQ </a> for more information.
	 */
	public double luminance() {
		return 0.2126 * getRed() + 0.7152 * getBlue() + 0.0722 * getGreen();
	}

	/**
	 * Calculates the distance between this color and a given color. This is
	 * useful when you want to process, for instance, images, where the colors
	 * in a region are similar but not all the same.
	 * 
	 * @param other
	 *            a color
	 * @return The Euclidean distance between this pixel and other, based on
	 *         their RGB values.
	 */
	public double dist(Color other) {
		final int rd = getRed() - other.getRed();
		final int bd = getBlue() - other.getBlue();
		final int gd = getGreen() - other.getGreen();
		return Math.sqrt(rd * rd + bd * bd + gd * gd);
	}

	/**
	 * @return an HTML-style #RRGGBB string
	 */
	public String getHTMLcolor() {
		return "#" + String.format("%X", getRGB());
	}

	public static void main(String[] args) {
		UberColor c = new UberColor(Color.PINK);
		System.out.printf("%s\n", c);
		System.out.printf("%s\n", c.getHTMLcolor());
		System.out.printf("%s", UberColor.fromHTMLcolor(c.getHTMLcolor()));
	}

}
