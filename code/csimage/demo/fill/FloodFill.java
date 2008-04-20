package csimage.demo.fill;

import java.awt.Color;
import java.awt.Point;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Random;
import java.util.TreeSet;

import csimage.UberImage;
import csimage.show;
import csimage.util.EasyInput;

public class FloodFill {

	public static void main(String[] args) {
		System.out.println("Looking for files in " + EasyInput.getcwd());
		UberImage m = UberImage.fromFile("pic.png");
		UberImage fillImage = UberImage.fromFile("csimage/pictures/sumo.jpg");

		// m.setColor(250, 240, Color.red);
		loopFloodFill(m, 300, 50, Color.WHITE, Color.RED);
		show.inFrame(m);
		// borderFloodFill(m, 1, 1, Color.BLACK, Color.RED);
		// randImageFloodFill(m, 300, 50, Color.BLACK, fillImage);
		System.out.println("Flood fill done");
		// m.show();
	}

	// Recursively fill an area starting at pixel (x, y).
	private static void recursiveFloodFill(UberImage m, int x, int y,
			Color bgColor, Color fillColor) {
		if (x < 0 || y < 0 || x >= m.getWidth() || y >= m.getHeight()) {
			return;
		} else if (!m.getColor(x, y).equals(bgColor)) {
			return;
		} else {
			m.setColor(x, y, fillColor);
			recursiveFloodFill(m, x, y - 1, bgColor, fillColor);
			recursiveFloodFill(m, x, y + 1, bgColor, fillColor);
			recursiveFloodFill(m, x - 1, y, bgColor, fillColor);
			recursiveFloodFill(m, x + 1, y, bgColor, fillColor);
		}
	}

	// This method is much more efficient than the recursive algorithm,
	// and it requies a few more lines of code. Yet it is a conceptually
	// straightforward algorithm, is probably easier to understand what
	// is going on since it doesn't have any confusing recursive calls.
	// Some of the messy parts are due to Java's imperfections.
	private static void loopFloodFill(UberImage m, int x, int y, Color bgColor,
			Color fillColor) {
		assert !bgColor.equals(fillColor);
		// if the (x, y) is not on the image, then there's nothing to do
		if (x < 0 || y < 0 || x >= m.getWidth() || y >= m.getHeight()) {
			return;
		} else if (!m.getColor(x, y).equals(bgColor)) {
			return;
		} else {
			// Objects stored in a TreeSet must be either naturally comparable,
			// or you must supply a Comparator object to compare them.
			// Unfortunately, Java's Point objects aren't naturally comparable,
			// so we must write a comparator. The exact comparison doesn't
			// really matter for flood fill, although different comparators can
			// result in the pixels being filled in different orders, which may
			// be odd or interesting for some users if the fill is slow.
			TreeSet paintMe = new TreeSet(new Comparator() {
				public int compare(Object o1, Object o2) {
					Point p1 = (Point) o1;
					Point p2 = (Point) o2;
					if (p1.equals(p2)) {
						return 0;
					} else if (p1.x == p2.x) {
						if (p1.y < p2.y) {
							return -1;
						} else {
							return 1;
						}
					} else if (p1.x < p2.x) {
						return -1;
					} else {
						return 1;
					}
				}
			});

			paintMe.add(new Point(x, y));

			while (paintMe.size() > 0) {
				Point p = (Point) paintMe.first();

				if (p.x < 0 || p.y < 0 || p.x >= m.getWidth()
						|| p.y >= m.getHeight()) {
					paintMe.remove(p);
				} else if (!bgColor.equals(m.getColor(p.x, p.y))) {
					paintMe.remove(p);
				} else {
					m.setColor(p.x, p.y, fillColor);
					paintMe.remove(p);

					Point north = new Point(p.x, p.y - 1);
					Point south = new Point(p.x, p.y + 1);
					Point east = new Point(p.x + 1, p.y);
					Point west = new Point(p.x - 1, p.y);
					paintMe.add(north);
					paintMe.add(south);
					paintMe.add(east);
					paintMe.add(west);
				}
			}
		}
	}

	// A modified version of loopFloodFill to use lines as fill-stoppers
	// instead of the background pixel color. This way anti-aliased images
	// (for instance) get filled without leaving little pock-marks of non-filled
	// background. One problem this introduces is that it is no longer trivial
	// to tell if a pixel has been filled, and so we use a HashSet to remember
	// all the points that have already been filled.
	private static void borderFloodFill(UberImage m, int x, int y,
			Color lineColor, Color fillColor) {
		// if the (x, y) is not on the image, then there's nothing to do
		if (x < 0 || y < 0 || x >= m.getWidth() || y >= m.getHeight()) {
			return;
		} else if (m.getColor(x, y).equals(lineColor)) {
			return;
		} else {
			// Objects stored in a TreeSet must be either naturally comparable,
			// or you must supply a Comparator object to compare them.
			// Unfortunately, Java's Point objects aren't naturally comparable,
			// so we must write a comparator. The exact comparison doesn't
			// really matter for flood fill, although different comparators can
			// result in the pixels being filled in different orders, which may
			// be odd or interesting for some users if the fill is slow.
			TreeSet paintMe = new TreeSet(new Comparator() {
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

			// seenIt keeps track of the pixels that have been painted
			HashSet seenIt = new HashSet();

			paintMe.add(new Point(x, y));
			UberImage fillImage = UberImage.fromFile("sumo.jpg");
			while (paintMe.size() > 0) {
				Point p = (Point) paintMe.first();
				if (p.x < 0 || p.y < 0 || p.x >= m.getWidth()
						|| p.y >= m.getHeight()) {
					paintMe.remove(p);
					seenIt.add(p);

					// this if-statement is different than the previous
					// algorithms!
				} else if (m.getColor(p.x, p.y).equals(lineColor)
						|| seenIt.contains(p)) {
					paintMe.remove(p);
					seenIt.add(p);
				} else {
					m.setColor(p.x, p.y, fillColor);
					paintMe.remove(p);
					seenIt.add(p);

					Point north = new Point(p.x, p.y - 1);
					Point south = new Point(p.x, p.y + 1);
					Point east = new Point(p.x + 1, p.y);
					Point west = new Point(p.x - 1, p.y);
					paintMe.add(north);
					paintMe.add(south);
					paintMe.add(east);
					paintMe.add(west);
				}
			}
		}
	}

	private static void imageFloodFill(UberImage m, int x, int y,
			Color lineColor, UberImage fillImage) {
		assert m.getWidth() <= fillImage.getWidth();
		assert m.getHeight() <= fillImage.getHeight();

		// if the (x, y) is not on the image, then there's nothing to do
		if (x < 0 || y < 0 || x >= m.getWidth() || y >= m.getHeight()) {
			return;
		} else if (m.getColor(x, y).equals(lineColor)) {
			return;
		} else {
			// Objects stored in a TreeSet must be either naturally comparable,
			// or you must supply a Comparator object to compare them.
			// Unfortunately, Java's Point objects aren't naturally comparable,
			// so we must write a comparator. The exact comparison doesn't
			// really matter for flood fill, although different comparators can
			// result in the pixels being filled in different orders, which may
			// be odd or interesting for some users if the fill is slow.
			TreeSet paintMe = new TreeSet(new Comparator() {
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

			// seenIt keeps track of the pixels that have been painted
			HashSet seenIt = new HashSet();

			paintMe.add(new Point(x, y));

			while (paintMe.size() > 0) {
				Point p = (Point) paintMe.first();
				if (p.x < 0 || p.y < 0 || p.x >= m.getWidth()
						|| p.y >= m.getHeight()) {
					paintMe.remove(p);
					seenIt.add(p);
				} else if (m.getColor(p.x, p.y).equals(lineColor)
						|| seenIt.contains(p)) {
					paintMe.remove(p);
					seenIt.add(p);
				} else {
					m.setColor(p.x, p.y, fillImage.getColor(p.x, p.y));
					paintMe.remove(p);
					seenIt.add(p);

					Point north = new Point(p.x, p.y - 1);
					Point south = new Point(p.x, p.y + 1);
					Point east = new Point(p.x + 1, p.y);
					Point west = new Point(p.x - 1, p.y);
					paintMe.add(north);
					paintMe.add(south);
					paintMe.add(east);
					paintMe.add(west);
				}
			}
		}
	}

	private static Random rnd = new Random();

	private static void randImageFloodFill(UberImage m, int x, int y,
			Color lineColor, UberImage fillImage) {
		assert m.getWidth() <= fillImage.getWidth();
		assert m.getHeight() <= fillImage.getHeight();

		// if the (x, y) is not on the image, then there's nothing to do
		if (x < 0 || y < 0 || x >= m.getWidth() || y >= m.getHeight()) {
			return;
		} else if (m.getColor(x, y).equals(lineColor)) {
			return;
		} else {
			// Objects stored in a TreeSet must be either naturally comparable,
			// or you must supply a Comparator object to compare them.
			// Unfortunately, Java's Point objects aren't naturally comparable,
			// so we must write a comparator. The exact comparison doesn't
			// really matter for flood fill, although different comparators can
			// result in the pixels being filled in different orders, which may
			// be odd or interesting for some users if the fill is slow.
			TreeSet paintMe = new TreeSet(new Comparator() {
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

			// seenIt keeps track of the pixels that have been painted
			HashSet seenIt = new HashSet();

			paintMe.add(new Point(x, y));

			while (paintMe.size() > 0) {
				Object[] arr = paintMe.toArray();
				Point p = (Point) arr[rnd.nextInt(arr.length)];
				// Point p = (Point) paintMe.first();
				if (p.x < 0 || p.y < 0 || p.x >= m.getWidth()
						|| p.y >= m.getHeight()) {
					paintMe.remove(p);
					seenIt.add(p);
				} else if (m.getColor(p.x, p.y).equals(lineColor)
						|| seenIt.contains(p)) {
					paintMe.remove(p);
					seenIt.add(p);
				} else {
					m.setColor(p.x, p.y, fillImage.getColor(p.x, p.y));
					paintMe.remove(p);
					seenIt.add(p);

					Point north = new Point(p.x, p.y - 1);
					Point south = new Point(p.x, p.y + 1);
					Point east = new Point(p.x + 1, p.y);
					Point west = new Point(p.x - 1, p.y);
					paintMe.add(north);
					paintMe.add(south);
					paintMe.add(east);
					paintMe.add(west);
				}
			}
		}
	}
}