package shapes;

import java.awt.Color;

import csimage.UberImage;

public class Line {

	private Point p;

	private Point q;

	private double slope;

	private int dx;

	private int dy;

	public Line(Point p, Point q) {
		this.p = p;
		this.q = q;
		this.dx = p.x - q.x;
		this.dy = p.y - q.y;
		this.slope = this.dy / (double) this.dx;
	}

	public Line(Line l) {
		this(l.p, l.q);
	}

	public Line(int a, int b, int x, int y) {
		this(new Point(a, b), new Point(x, y));
	}

	public Point getP() {
		return this.p;
	}

	public Point getQ() {
		return this.q;
	}

	public double getSlope() {
		return this.slope;
	}

	public void drawOn(UberImage img) {
		int start = Math.min(this.p.x, this.q.x);
		int end = Math.max(this.p.x, this.q.x);
		double y = this.p.y;
		if (this.dx == 0) {
			for (int yy = this.p.y; yy <= this.q.y; ++yy) {
				img.setColor(this.p.x, yy, Color.BLACK);
			}
		} else if (-1 <= this.slope && this.slope <= 1) {
			for (int x = start; x <= end; ++x) {
				img.setColor(x, (int) (y + 0.5), Color.BLACK);
				y += this.slope;
			}
		} else {
			for (int x = start; x <= end; ++x) {
				img.setColor((int) (y + 0.5), x, Color.BLACK);
				y += this.slope;
			}
		}
	}

	@Override
	public String toString() {
		return String.format("[P=%s, Q=%s, %s]", this.p, this.q, this.slope);
	}

}
