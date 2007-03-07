package objects.shape;

import java.awt.Color;
import java.util.ArrayList;

public class PointTest {

	public static void main(String[] args) {
		Point p = new Point();
		p.print();
		System.out.println(p.toString());
		p.setX(-54);

		p.setY(p.getX() * p.getX() + 12);

		p.print();
		
		ArrayList<Point> pts = new ArrayList<Point>();
		ColorPoint q = new ColorPoint(45, 2, Color.RED);
		for(int i = 0; i < 10; ++i) {
			pts.add(q);
		}
		System.out.println(pts.toString());
		Point r = pts.get(3);
		r.setX(-5);
		r.setY(43);

		System.out.println(pts.toString());
	}

}
