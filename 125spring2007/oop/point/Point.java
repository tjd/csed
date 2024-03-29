package oop.point;

public class Point {

	private static int count = 0;

	private int x;

	private int y;

	// constructors

	// default constructor
	public Point() {
		this(0, 0);
	}

	// copy constructor
	public Point(Point p) {
		this(p.x, p.y);
	}

	public Point(int x, int y) {
		this.x = x;
		this.y = y;
		++count;
	}

	// return number of Point objects that have been created
	public int getCount() {
		return count;
	}

	// setters and getters
	public int getX() {
		return x;
	}

	public void setX(int x) {
		this.x = x;
	}

	public int getY() {
		return y;
	}

	public void setY(int y) {
		this.y = y;
	}

	// other methods
	@Override
	public String toString() {
		return String.format("(%s, %s)", this.x, this.y);
	}

	public double dist(Point other) {
		int dx = this.x - other.x;
		int dy = this.y - other.y;
		return Math.sqrt(dx * dx + dy * dy);
	}

	public boolean equals(Object x) {
		if (x instanceof Point) {
			Point other = (Point) x;
			return this.x == other.x && this.y == other.y;
		} else {
			// might be better to throw an error if you try to compare a Point
			// to a non-Point
			return false;
		}
	}

	//
	// simple test to make sure it works
	//
	public static void main(String[] args) {
		Point origin = new Point();
		Point p1 = new Point(2, 45);
		Point q = new Point(p1);
		q.setY(-3);

		System.out.printf("origin = %s\n", origin);
		System.out.printf("p1 = %s\n", p1);
		System.out.printf("q = %s\n", q);
		System.out.printf("origin.dist((1, 1)) = %s\n", origin.dist(new Point(
				1, 1)));
	}

}
