package pylist;

public class PyListTest extends TestCase {

	public static void testAppend1() {
		PyList lst = new PyList();
		assertTrue(lst.size() == 0);
		lst.append(5);
		assertTrue(lst.size() == 1);
		assertTrue(lst.get(0) == 5);
		lst.append(2);
		assertTrue(lst.size() == 2);
		assertTrue(lst.get(1) == 2);
		lst.append(25);
		assertTrue(lst.size() == 3);
		assertTrue(lst.get(2) == 25);
	}

	public static void testAppend2() {
		PyList lst = new PyList();

		for (int i = 0; i < 11; ++i) {
			assertTrue(lst.size() == i);
			lst.append(i);
		}
		assertTrue(lst.size() == 11);
	}

	public static void testEquals() {
		PyList a = new PyList();
		PyList b = new PyList();
		assertTrue(a.equals(b));
		a.append(1);
		assertFalse(a.equals(b));
		b.append(1);
		assertTrue(a.equals(b));
		for (int i = -15; i < 6; ++i) {
			a.append(i);
			b.append(i);
		}
		assertTrue(a.equals(b));
	}

	public static void testReverse() {
		PyList a = new PyList();
		PyList aRev = new PyList();
		a.reverse();
		assertTrue(a.equals(aRev));

		PyList b = new PyList();
		b.append(7);
		PyList bRec = new PyList();
		bRec.append(7);
		b.reverse();
		assertTrue(b.equals(bRec));

		PyList c = new PyList();
		c.append(8);
		c.append(7);
		PyList cRec = new PyList();
		cRec.append(7);
		cRec.append(8);
		c.reverse();
		assertTrue(String.format("b = %s, bRec = %s", c, cRec), c.equals(cRec));

		PyList d = new PyList();
		PyList dRec = new PyList();
		for (int i = 0; i < 12; ++i) {
			d.append(i);
			dRec.append(11 - i);
		}
		d.reverse();
		assertTrue(d.equals(dRec));
	}

	public static void main(String[] args) {
		junit.swingui.TestRunner.run(PyListTest.class);
	}

}
