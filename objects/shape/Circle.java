package objects.shape;

public class Circle {

private Point center;

	private int radius; // radius > 0

	private Circle(int x, int y, int radius) {
		if (radius <= 0) {
			throw new Error("Circle's radius must be positive");
		} else {
			this.center = new Point(x, y);
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
		return center.getX();
	}

	public int getY() {
		return center.getY();
	}

}
