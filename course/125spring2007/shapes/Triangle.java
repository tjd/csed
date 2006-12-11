package shapes;

import csimage.UberImage;

public class Triangle {

	private Line ab;

	private Line bc;

	private Line ac;

	public Triangle(Point a, Point b, Point c) {
		ab = new Line(a, b);
		bc = new Line(b, c);
		ac = new Line(a, c);
	}

	public Triangle(int x, int y, int a, int b, int m, int n) {
		this(new Point(x, y), new Point(a, b), new Point(m, n));
	}

	public Triangle(Triangle tri) {
		this(tri.getA(), tri.getB(), tri.getC());
	}

	public Point getA() {
		return ab.getP();
	}

	public Point getB() {
		return ab.getQ();
	}

	public Point getC() {
		return bc.getQ();
	}

	public void drawOn(UberImage img) {
		ab.drawOn(img);
		bc.drawOn(img);
		ac.drawOn(img);
	}

	public String toString() {
		return String.format("[A:%s, B:%s, C:%s]", getA(), getB(), getC());
	}

	public String debug() {
		return String.format("ab=%s\nbc=%s\nac=%s", ab, bc, ac);
	}
	
}
