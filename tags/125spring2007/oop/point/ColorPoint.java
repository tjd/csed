package oop.point;

import java.awt.Color;


public class ColorPoint extends Point {

	private Color color;

	public ColorPoint() {
		this(0, 0, Color.BLACK);
	}

	public ColorPoint(int x, int y, Color c) {
		super(x, y);
		color = c;
	}

	@Override
	public String toString() {
		return String.format("(%s, %s, %s)", getX(), getY(), color);
	}

}
