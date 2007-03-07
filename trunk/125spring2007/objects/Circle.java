package objects;

public class Circle {

	private int x;

	private int y;

	private int radius; // radius > 0

	private Circle(int x, int y, int radius) {
		if (radius <= 0) {
			throw new Error("Circle's radius must be positive");
		} else {
			this.x = x;
			this.y = y;
			this.radius = radius;
		}
	}
	
	public Circle(Point center, int radius) {
		this(center.getX(), center.getY(), radius);
	}
	
	public double area() {
		return Math.PI * radius * radius;
	}

	public int getRadius() {
		return radius;
	}

	public int getX() {
		return x;
	}

	public int getY() {
		return y;
	}

}
