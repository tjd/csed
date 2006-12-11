package csimage.util;

import java.awt.Color;

public interface PixelMapper {
	/**
	 * Returns a new color for the pixel at location (x, y).
	 * 
	 * @param x
	 *            x-coordinate of the pixel
	 * @param y
	 *            y-coordinate of the pixel
	 * @param c
	 *            current color of the pixel
	 * @return new color of the pixel
	 */
	public Color trans(int x, int y, Color c);
}