package csimage.demo.fill;

import java.awt.Color;
import java.awt.Point;
import java.util.Comparator;
import java.util.TreeSet;

import csimage.UberImage;
import csimage.show;
import csimage.util.EasyInput;
import csimage.util.dbg;

public class ScanFill {

	public static void main(String[] args) {
		// test1();
		// test2();
		test3();
	}

	private static void test3() {
		dbg.DEBUG = false;
		System.out.println(EasyInput.getcwd());

		UberImage m = UberImage
				.fromFile("C:\\Documents and Settings\\tjd\\Desktop\\workspace\\cmpt125\\pic2.png");

		EasyInput.input("(press a key)");
		scanFill1(false, m, new Point(300, 30), Color.WHITE, Color.RED);
		show.inFrame(m);

		System.out.println("Flood fill done");
	}

	// CLIFFHANGER: Appears to work okay filling shapes with strict, but
	// crashes when it hits an image edge. So add edge-checking code.
	// Can TheMatrix's pixel repaint methods be made faster, e.g. just re-paing
	// a single pixel instead of the entire image?

	private static void scanFill1(boolean step, UberImage m, Point startSeed,
			Color bgColor, Color fillColor) {
		assert !bgColor.equals(fillColor);
		assert m.getColor(startSeed).equals(bgColor);

		TreeSet seedSet = new TreeSet(new Comparator() {
			public int compare(Object o1, Object o2) {
				Point p1 = (Point) o1;
				Point p2 = (Point) o2;
				if (p1.equals(p2)) {
					return 0;
				} else if (p1.x == p2.x) {
					return (p1.y < p2.y) ? -1 : 1;
				} else {
					return (p1.x < p2.x) ? -1 : 1;
				}
			}
		});

		seedSet.add(startSeed);

		while (seedSet.size() > 0) {
			dbg.sayln("seedSet = " + seedSet);
			Point left = (Point) seedSet.first();
			Point right = new Point(left.x + 1, left.y);
			seedSet.remove(left);
			dbg.sayln("seedSet = " + seedSet);
			assert m.getColor(left).equals(bgColor) : "" + left + "="
					+ m.getColor(left) + " == " + bgColor + "\n" + seedSet;

			dbg.sayln("filling left ...");
			fill(step, m, left, -1, bgColor, fillColor); // left

			dbg.sayln("filling right ...");
			fill(step, m, right, 1, bgColor, fillColor); // right

			dbg.sayln("seeding left ...");
			seed(m, left, -1, bgColor, fillColor, seedSet); // left

			dbg.sayln("seeding right ...");
			seed(m, left, 1, bgColor, fillColor, seedSet); // right
		}
	}

	private static void fill(boolean step, UberImage m, Point rawSeed, int dx,
			Color bgColor, Color fillColor) {
		Point seed = new Point(rawSeed);
		dbg.sayln("dx = " + dx + ", rawSeed = " + rawSeed);
		for (; m.getColor(seed).equals(bgColor); seed.translate(dx, 0)) {
			dbg.say("+");
			m.setColor(seed, fillColor);
			if (step) {
				EasyInput.input("(any key to continue)");
			}
		}
	}

	private static void seed(UberImage m, Point rawSeed, int dx, Color bgColor,
			Color fillColor, TreeSet seedSet) {
		dbg.sayln("dx = " + dx + ", rawSeed = " + rawSeed);
		assert !bgColor.equals(fillColor);
		assert m.getColor(rawSeed).equals(fillColor) : "" + m.getColor(rawSeed)
				+ " == " + fillColor;

		Point seed = new Point(rawSeed);

		if (seed.y - 1 < 0 || seed.y + 1 >= m.getHeight()) {
			return;
		} else {
			boolean aboveRegionSeeded = false;
			boolean belowRegionSeeded = false;

			Point above = new Point(seed);
			above.translate(0, -1);

			Point prevAbove = new Point(above);
			prevAbove.translate(-dx, 0);

			Point below = new Point(seed);
			below.translate(0, 1);

			Point prevBelow = new Point(below);
			prevBelow.translate(-dx, 0);

			// above
			if (m.getColor(above).equals(bgColor)) {
				seedSet.add(new Point(above));
				dbg.sayln("seed added above: " + above);
				// m.setColor(above, Color.YELLOW);
				aboveRegionSeeded = true;
			}

			// below
			if (m.getColor(below).equals(bgColor)) {
				seedSet.add(new Point(below));
				dbg.sayln("seed added below: " + below);
				// m.setColor(below, Color.YELLOW);
				belowRegionSeeded = true;
			}

			seed.translate(dx, 0);
			for (; m.getColor(seed).equals(fillColor); seed.translate(dx, 0)) {
				dbg.say(".");
				// move reference points
				above.translate(dx, 0);
				prevAbove.translate(dx, 0);
				below.translate(dx, 0);
				prevBelow.translate(dx, 0);

				// check above
				if (m.getColor(above).equals(bgColor)) {
					if (!aboveRegionSeeded) {
						dbg.sayln("seed added above: " + above);
						seedSet.add(new Point(above));
						// m.setColor(below, Color.YELLOW);
						aboveRegionSeeded = true;
					} else if (aboveRegionSeeded
							&& !m.getColor(prevAbove).equals(bgColor)) {
						aboveRegionSeeded = false;
					}
				}

				// check below
				if (m.getColor(below).equals(bgColor)) {
					if (!belowRegionSeeded) {
						dbg.sayln("seed added below: " + below);
						seedSet.add(new Point(below));
						// m.setColor(below, Color.YELLOW);
						belowRegionSeeded = true;
					} else if (belowRegionSeeded
							&& !m.getColor(prevBelow).equals(bgColor)) {
						belowRegionSeeded = false;
					}
				}
			} // for
		} // if
	}
}