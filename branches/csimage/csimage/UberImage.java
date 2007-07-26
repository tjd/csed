package csimage;

/**
 * For more information on reading/writing images, see
 * 
 *  - http://mindprod.com/jgloss/imageio.html
 *  - http://java.sun.com/j2se/1.5.0/docs/guide/imageio/spec/apps.fm1.html
 *  
 */


import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.IndexColorModel;
import java.awt.image.WritableRaster;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Hashtable;

import javax.imageio.ImageIO;

import csimage.util.dbg;

public class UberImage extends BufferedImage {

	private static final int DEFAULT_IMAGE_TYPE = UberImage.TYPE_INT_RGB;

	private static final UberColor DEFAULT_BACKGROUND_COLOR = UberColor.WHITE;

	public UberImage(ColorModel cm, WritableRaster raster,
			boolean isAlphaPremultiplied, Hashtable<?, ?> properties) {
		super(cm, raster, isAlphaPremultiplied, properties);
	}

	public UberImage(int width, int height, int imageType, IndexColorModel cm) {
		super(width, height, imageType, cm);
	}

	public UberImage(int width, int height, int imageType) {
		super(width, height, imageType);
	}

	/**
	 * Creates a new instance of BufferedImage. Makes a copy of the data of the
	 * passed-in image! Does not copy the image's properties (if any).
	 * 
	 * @param img
	 *            the image to copy
	 */
	public UberImage(BufferedImage img) {
		this(img.getColorModel(), img.copyData(null), img
				.isAlphaPremultiplied(), null);
	}

	/**
	 * Creates an image of the given dimensions with all pixels set to be the
	 * background color.
	 * 
	 */
	public static UberImage blankImage(int width, int height, Color background) {
		return blankImage(width, height, new UberColor(background));
	}

	/**
	 * Creates an image of the given dimensions with all pixels set to white.
	 * 
	 */
	public static UberImage blankImage(int width, int height) {
		return blankImage(width, height, DEFAULT_BACKGROUND_COLOR);
	}

	/**
	 * Creates an image of the given dimensions with all pixels set to be the
	 * background color.
	 * 
	 */
	public static UberImage blankImage(int width, int height,
			UberColor background) {
		UberImage img = new UberImage(width, height, DEFAULT_IMAGE_TYPE);
		img.fill(background);
		return img;
	}

	/**
	 * Creates an instance of UberImage from the given file path.
	 * 
	 * @param file
	 *            The image file to create the image from
	 * @throws FileNotFoundException
	 */
	public static UberImage fromFile(File file) {
		try {
			return new UberImage(ImageIO.read(file));
		} catch (IOException e) {
			throw new Error(String.format("File \"%s\" not found.", file
					.getAbsolutePath()));
		}
	}

	/**
	 * Creates an instance of UberImage from the given path.
	 * 
	 * @param file
	 *            A string representation of the image file to be loaded, e.g.
	 *            "csjava/pictures/lake1.jpg"
	 * @throws FileNotFoundException
	 */
	public static UberImage fromFile(String path) {
		return fromFile(new File(path));
	}

	/**
	 * 
	 * @param url
	 * @return a new UberImage downloaded from the given URL
	 */
	public static UberImage fromURL(URL url) {
		try {
			return new UberImage(ImageIO.read(url));
		} catch (IOException e) {
			throw new Error(String.format("Unable to load \"%s\".", url
					.getPath()));
		}
	}

	/**
	 * 
	 * @param url
	 * @return a new UberImage downloaded from the given URL
	 */
	public static UberImage fromURL(String url) {
		try {
			return fromURL(new URL(url));
		} catch (MalformedURLException e) {
			throw new Error(String.format("\"%s\" is malformed", url));
		}
	}

	/**
	 * 
	 * @param img
	 * @return a copy of the given image
	 */
	public static UberImage fromImage(BufferedImage img) {
		return new UberImage(img);
	}

	/**
	 * Saves the image as a PNG file.
	 * 
	 * @param path
	 *            name/path to save the image in
	 */
	public void saveAsPNG(String path) {
		saveAsPNG(new File(path));
	}

	/**
	 * Saves the image as a PNG file.
	 * 
	 * @param file
	 *            the file to save the image in
	 */
	public void saveAsPNG(File file) {
		try {
			ImageIO.write(this, "png", file);
		} catch (IOException e) {
			throw new Error(String.format("File \"%s\" not found.", file
					.getAbsolutePath()));
		}
	}

	/**
	 * Saves the image as a JPEG file.
	 * 
	 * @param path
	 *            name/path to save the image in
	 */
	public void saveAsJPEG(String path) {
		saveAsJPEG(new File(path));
	}

	/**
	 * Saves the image as a JPEG file.
	 * 
	 * @param path
	 *            the file to save the image in
	 */
	public void saveAsJPEG(File file) {
		try {
			ImageIO.write(this, "jpeg", file);
		} catch (IOException e) {
			throw new Error(String.format("File \"%s\" not found.", file
					.getAbsolutePath()));
		}
	}

	/**
	 * Saves the image as a BMP file.
	 * 
	 * @param path
	 *            name/path to save the image in
	 */
	public void saveAsBMP(String fname) {
		saveAsBMP(new File(fname));
	}

	/**
	 * Saves the image as a BMP file.
	 * 
	 * @param path
	 *            the file to save the image in
	 */
	public void saveAsBMP(File file) {
		try {
			ImageIO.write(this, "bmp", file);
		} catch (IOException e) {
			throw new Error(String.format("File \"%s\" not found.", file
					.getAbsolutePath()));
		}
	}

	/**
	 * Verifies that pixel (x,y) is in this image.
	 * 
	 * @param x
	 *            x-coordinate of a pixel
	 * @param y
	 *            y-coordinate of a pixel
	 */
	private void assertCheckRange(int x, int y) {
		assert 0 <= x : "\n" + dbg.pair(x, y) + " - x coordinate must be >= 0";
		assert x < getWidth() : "\n" + dbg.pair(x, y)
				+ " - x coordinate must be <  " + getWidth();
		assert 0 <= y : "\n" + dbg.pair(x, y) + " - y coordinate must be >= 0";
		assert y < getHeight() : "\n" + dbg.pair(x, y)
				+ " - y coordinate must be <  " + getHeight();
	}

	private void assertCheckRange(Point p) {
		assertCheckRange(p.x, p.y);
	}

	/**
	 * @return true if (x, y) is within the current image, false otherwise
	 */
	public boolean inImage(int x, int y) {
		return x >= 0 && y >= 0 && x < getWidth() && y < getHeight();
	}

	/**
	 * @return true if p is within the current image, false otherwise
	 */
	public boolean inImage(Point p) {
		return inImage(p.x, p.y);
	}

	/**
	 * @return A Graphics2D object that can be used to draw on top of the image.
	 */
	public Graphics2D getGraphics2D() {
		return createGraphics();
	}

	/**
	 * Sets the pixel at (x, y) to be color c.
	 * 
	 * @param x
	 *            x-coordinate of a pixel
	 * @param y
	 *            y-coordinate of a pixel
	 * @param c
	 *            color to be set at (x, y) position
	 */
	public void setColor(int x, int y, Color c) {
		assertCheckRange(x, y);
		setRGB(x, y, c.getRGB());
	}

	public void setColor(int x, int y, int red, int green, int blue) {
		setColor(x, y, new UberColor(red, green, blue));
	}

	public void setColor(Point p, Color c) {
		setColor(p.x, p.y, c);
	}

	/**
	 * @return the color of the pixel at location (x, y)
	 */
	public UberColor getColor(int x, int y) {
		try {
			return new UberColor(getRGB(x, y));
		} catch (ArrayIndexOutOfBoundsException e) {
			throw new Error("Array Index out of bounds error inside getColor: "
					+ dbg.pair("" + x, "" + y));
		} // try
	}

	public UberColor getColor(Point p) {
		return getColor(p.x, p.y);
	}

	/**
	 * Sets every pixel to be color c.
	 * 
	 * @param c
	 *            the fill color
	 */
	public void fill(Color c) {
		for (int i = 0; i < getWidth(); ++i) {
			for (int j = 0; j < getHeight(); j++) {
				setColor(i, j, c);
			} // for
		} // for
	}

	/**
	 * Color in the rectangle with upper-left corner(a, b) and given width and
	 * height.
	 * 
	 * @param a
	 *            x coordinate of the upper-left corner of the rectangle to fill
	 * @param b
	 *            y coordinate of the upper-left corner of the rectangle to fill
	 * @param width
	 *            the rectangle's width (in pixels)
	 * @param height
	 *            the rectangle's height (in pixels)
	 */
	public void fillRectangle(int a, int b, int width, int height, Color c) {
		assert width >= 0 : "width is >= 0";
		assert height >= 0 : "height is >= 0";
		assertCheckRange(a, b);
		assertCheckRange(a + width - 1, b + height - 1);

		for (int x = a; x < a + width; x++) {
			for (int y = b; y < b + height; y++) {
				setColor(x, y, c);
			}
		}
	}

	/**
	 * Color in the rectangle with upper-left corner p and side-length side.
	 * 
	 * @param p
	 *            upper-left corner of the rectangle
	 * @param width
	 *            the rectangle's width (in pixels)
	 * @param height
	 *            the rectangle's height (in pixels)
	 */
	public void fillRectangle(Point p, int width, int height, Color c) {
		fillRectangle(p.x, p.y, width, height, c);
	}

	/**
	 * Color in the square with upper-left corner (x, y) and side-length side.
	 * 
	 * @param side
	 *            the length (in pixels) of the side of the square
	 */
	public void fillSquare(int x, int y, int side, Color c) {
		fillRectangle(x, y, side, side, c);
	}

	/**
	 * Color in the square with upper-left corner p and side-length side.
	 * 
	 * @param p
	 *            the upper-left corner of the square
	 * @param side
	 *            the length (in pixels) of the side of the square
	 */
	public void fillSquare(Point p, int side, Color c) {
		fillRectangle(p, side, side, c);
	}

	/**
	 * Applies the given pixel transformation to every pixel in the specified
	 * rectangle. The point ul is the upper-left corner of the rectangle, and lr
	 * is the lower right.
	 * 
	 * @param f
	 *            the pixel transformation
	 */
	public void mapPixel(UberPixelMapper f, Point ul, Point lr) {
		assertCheckRange(ul);
		assertCheckRange(lr);
		for (int x = ul.x; x < lr.x; x++) {
			for (int y = ul.y; y < lr.y; y++) {
				Color newPixelColor = f.trans(x, y, getColor(x, y));
				setColor(x, y, newPixelColor);
			}
		}
	}

	/**
	 * Applies the given pixel transformation to every pixel in the image.
	 * 
	 * @param f
	 *            the pixel transformation
	 */
	public void mapPixel(UberPixelMapper f) {
		mapPixel(f, new Point(0, 0), new Point(getWidth(), getHeight()));
	}

}
