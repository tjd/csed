package shapes;

import csimage.UberImage;

public class Triangle {

	private Line ab;

	private Line bc;

	private Line ac;

	public Triangle(Point a, Point b, Point c) {
		this.ab = new Line(a, b);
		this.bc = new Line(b, c);
		this.ac = new Line(a, c);
	}

	public Triangle(int x, int y, int a, int b, int m, int n) {
		this(new Point(x, y), new Point(a, b), new Point(m, n));
	}

	public Triangle(Triangle tri) {
		this(tri.getA(), tri.getB(), tri.getC());
	}

	public Point getA() {
		return this.ab.getP();
	}

	public Point getB() {
		return this.ab.getQ();
	}

	public Point getC() {
		return this.bc.getQ();
	}

	public void drawOn(UberImage img) {
		this.ab.drawOn(img);
		this.bc.drawOn(img);
		this.ac.drawOn(img);
	}

	@Override
	public String toString() {
		return String.format("[A:%s, B:%s, C:%s]", this.getA(), this.getB(),
				this.getC());
	}

	public String debug() {
		return String.format("ab=%s\nbc=%s\nac=%s", this.ab, this.bc, this.ac);
	}

}
