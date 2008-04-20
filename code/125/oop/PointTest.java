package oop;

public class PointTest {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		util.Util.ensureAssertionsEnabled();
		Point p = new Point();
		assert p.getX() == 0;
		assert p.getY() == 0;
		assert p.toString().equals("(0, 0)");
		p.setX(5);
		assert p.getX() == 5;
		assert p.getY() == 0;
		assert p.toString().equals("(5, 0)");
		p.setY(-4);
		assert p.getX() == 5;
		assert p.getY() == -4;
		assert p.toString().equals("(5, -4)");
		System.out.println("All Point test passed!");
	}

}
