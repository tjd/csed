package csimage;


public interface UberPixelMapper {
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
	public UberColor trans(int x, int y, UberColor c);
}