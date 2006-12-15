package shapes;

import java.util.Random;

import csimage.UberImage;
import csimage.show;

public class ShapeTest {
	static Random rnd = new Random();

	private static void test3() {
		UberImage img = UberImage.blankImage(500, 500);
		Triangle tri = new Triangle(10, 10, 40, 450, 225, 400);
		System.out.printf("tri = %s\n", tri);
		System.out.printf(tri.debug());
		tri.drawOn(img);
		show.inFrame(img);
	}
	
	private static void test2() {
		UberImage img = UberImage.blankImage(500, 500);
		Line line1 = new Line(10, 10, 200, 200);
		line1.drawOn(img);
		
		Line line2 = new Line(300, 300, 200, 450);
		line2.drawOn(img);
		show.inFrame(img);
	}
	
	public static void test1() {
		UberImage img = UberImage.blankImage(500, 500);
		for (int i = 0; i < 500; ++i) {
			for (int j = 0; j < 500; ++j) {
				Point p = new Point(i, j);
				if (rnd.nextDouble() < .1) {
					p.drawOn(img);
				}
			}
		}
		show.inFrame(img);
	}

	public static void main(String[] args) {
//		test1();
//		test2();
		test3();
	}


}
