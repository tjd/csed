package oop;

public class Circle {

	private int x;
	private int y;
	private int radius;

	public Circle(int a, int b, int r) {
		if (r <= 0) {
			throw new Error("Circle constructor error: radius must be &gt; 0");
		} else {
			x = a;
			y = b;
			radius = r;
		}
	}

	public void print() {
		System.out.print("[Circle: center=(" + x + "," + y + "), radius="
				+ radius + "]");
	}

	public int getX() {
		return x;
	}

	public int getY() {
		return y;
	}

	public int getRadius() {
		return radius;
	}

	public int getDiameter() {
		return 2 * radius;
	}

	public double area() {
		return Math.PI * radius * radius;
	}
}
