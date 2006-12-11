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
		dx = p.x - q.x;
		dy = p.y - q.y;
		slope = dy / (double)dx;
	}

	public Line(Line l) {
		this(l.p, l.q);
	}

	public Line(int a, int b, int x, int y) {
		this(new Point(a, b), new Point(x, y));
	}

	public Point getP() {
		return p;
	}

	public Point getQ() {
		return q;
	}

	public double getSlope() {
		return slope;
	}

	public void drawOn(UberImage img) {
		int start = Math.min(p.x, q.x);
		int end = Math.max(p.x, q.x);
		double y = p.y;
		if (dx == 0) {
			for (int yy = p.y; yy <= q.y; ++yy) {
				img.setColor(p.x, yy, Color.BLACK);
			}
		} else if (-1 <= slope && slope <= 1) {
			for (int x = start; x <= end; ++x) {
				img.setColor(x, (int) (y + 0.5), Color.BLACK);
				y += slope;
			}
		} else {
			for (int x = start; x <= end; ++x) {
				img.setColor((int) (y + 0.5), x, Color.BLACK);
				y += slope;
			}
		}
	}

	public String toString() {
		return String.format("[P=%s, Q=%s, %s]", p, q, slope);
	}

}
