package objects;

public class Point {
	private int x;

	private int y;

	// default constructor
	public Point() {
		this(0, 0);
	}

	// constructor
	public Point(int x, int y) {
		this.x = x;
		this.y = y;
	}

	// copy constructor
	public Point(Point p) {
		this(p.x, p.y);
	}

	// mutator
	public void translate(int dx, int dy) {
		this.x += dx;
		this.y += dy;
	}

	// doesn't change state (not a mutator)
	public double dist(Point p) {
		int dx = this.x - p.x;
		int dy = this.y - p.y;
		return Math.sqrt(dx * dx + dy * dy);
	}

	// doesn't change state (not a mutator)
	public boolean same(Point p) {
		return (this.x == p.x) && (this.y == p.y);
	}

	// doesn't change state (not a mutator)
	public String toString() {
		return "(" + this.x + ", " + this.y + ")";
	}

	// doesn't change state (not a mutator)
	public void print() {
		System.out.println(toString());
	}

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
}
