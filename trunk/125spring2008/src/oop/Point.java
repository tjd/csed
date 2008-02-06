package oop;

class Point {
	private int x;
	private int y;

	public Point() { // default constructor
		this(0, 0);
	}

	public Point(Point p) { // copy constructor
		this(p.x, p.y);
	}

	public Point(int a, int b) {
		x = a;
		y = b;
	}

	public void translate(int dx, int dy) {
        x += dx;
        y += dy;
    }

	public void print() {
		System.out.printf("(%s, %s)", x, y);
	}
}