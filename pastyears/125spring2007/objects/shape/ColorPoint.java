package objects.shape;

import java.awt.Color;

public class ColorPoint extends Point {

	private Color c;

	public ColorPoint(int x, int y, Color c) {
		super(x, y);
		this.c = c;
	}

	public Color getColor() {
		return c;
	}

	public void setColor(Color c) {
		this.c = c;
	}

	public String toString() {
		return "(" + this.getX() + ", " + this.getY() + "," + this.c + ")";
	}
}
