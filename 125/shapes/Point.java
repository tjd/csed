package shapes;

import java.awt.Color;

import csimage.UberImage;

/*
 * Point.java
 * 
 * Stores an (x, y) point on the screen, where x and y are integers.
 * 
 */

public class Point {

	public int x;

	public int y;

	public Point() {
		this(0, 0);
	}

	public Point(int x, int y) {
		this.x = x;
		this.y = y;
	}

	public Point(Point p) {
		this.x = p.x;
		this.y = p.y;
	}

	public void drawOn(UberImage img) {
		img.setColor(x, y, Color.BLACK);
	}

	public String toString() {
		return String.format("(%s, %s)", x, y);
	}

}
