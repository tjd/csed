package polynomial;

import java.util.ArrayList;
import java.util.Iterator;

/*
 * This is based in part on the Poly class from the book Program Development in Java, by 
 * Barbara Liskov.
 * 
 */

public class Poly {

	private ArrayList<Term> terms;

	private int deg;

	// creates the zero polynomial, i.e. a polynomial whose coefficients
	// are all zero and has degree zero
	public Poly() {
		terms = new ArrayList<Term>();
		deg = 0;
	}

	// Creates a monomial of the form cx^n.
	public Poly(int c, int n) {
		terms = new ArrayList<Term>();
		terms.add(new Term(c, n));
		if (c == 0) {
			deg = 0;
		} else {
			deg = n;
		}
	}

	// Creates a new copy of polynomial p.
	public Poly(Poly p) {
		terms = new ArrayList<Term>();
		deg = p.deg;
		for (Term t : p.terms) {
			terms.add(t);
		}
	}

	// Creates a zero polynomial with n + 1 coefficients all set to 0. This is
	// a helper constructor only meant for use by the Poly class.
	private Poly(int n) {
		deg = n;
		terms = new ArrayList<Term>(n);
	}

	// Returns the degree of this polynomial, i.e. the largest exponent
	// with a non-zero coefficient. Returns 0 for the zero polynomial.
	public int degree() {
		return deg;
	}

	// Returns the coefficient of the term with exponent d.
	public int coeff(int d) {
		if (d < 0 || d > deg) {
			return 0;
		} else {
			return terms.get(d).coeff;
		}
	}

	// Returns a new polynomial that is the sum of p and this polynomial.
	// The polynomials are store as pairs of terms, from smallest coefficient
	// to largest. Thus adding two polynomials turns out to be a variation
	// of merging.
	public Poly add(Poly p) {
		int a = 0; // a and b point to the first unprocessed term in
		int b = 0; // in this and p respectively
		int A = this.terms.size();
		int B = p.terms.size();
		Poly result = new Poly();
		while (a < A || b < B) {
			if (a >= A) {
				// debug("a >= A");
				result.terms.add(p.terms.get(b));
				++b;
			} else if (b >= B) {
				// debug("b >= B");
				result.terms.add(this.terms.get(a));
				++a;
			} else {
				Term termA = this.terms.get(a);
				Term termB = p.terms.get(b);
				if (termA.pow == termB.pow) {
					// debug("termA.pow == termB.pow");
					result.terms.add(new Term(termA.coeff + termB.coeff,
							termA.pow));
					++a;
					++b;
				} else if (termA.pow < termB.pow) {
					// debug("termA.pow < termB.pow");
					result.terms.add(termA);
					++a;
				} else if (termA.pow > termB.pow) {
					// debug("termA.pow > termB.pow");
					result.terms.add(termB);
					++b;
				} else {
					assert 1 == 2 : "something impossible has happened!";
				}
			}
		}
		debug("" + result.terms);
		return result;
	}

	// Returns a new polynomial that is the product of p and this polynomial.
	public Poly mult(Poly p) {
		Poly r = new Poly();
		for (int i = 0; i < this.terms.size(); ++i) {
			Poly c = p.scalarMult(this.terms.get(i).coeff);
			r = r.add(c);
		}
		return r;
	}

	// Returns a new polynomial that is the differnecet of p and this
	// polynomial,
	// i.e. this - p.
	public Poly sub(Poly p) {
		return add(p.minus());
	}

	// Returns a new polynomial that is a copy of this, but with all
	// coefficients multiplied by s.
	public Poly scalarMult(int s) {
		Poly p = new Poly(this);
		for (Term t : terms) {
			p.terms.add(new Term(s * t.coeff, t.pow));
		}
		return p;
	}

	// Returns a new polynomial that is the same as this one, but with all
	// coefficients multiplied by -1.
	public Poly minus() {
		return scalarMult(-1);
	}

	public String toString() {
		return "" + terms;
	}

	public static void main(String[] args) {
		test1();
		test2();
		// Term t1 = new Term(2, 2);
		// Term t2 = new Term(10, 1);
		// //System.out.println(t1);
		// //System.out.println(t2);
		//
		// Poly a = new Poly(1, 2);
		// Poly b = new Poly(3, 1);
		// Poly c = new Poly(4, 0);
		//
		// System.out.println("" + a);
		// System.out.println("" + b);
		// System.out.println("" + c);
		// System.out.println("" + a.add(b));
	}

	public static void test1() {
		Poly a = new Poly(1, 2);
		Poly b = new Poly(3, 1);
		Poly c = new Poly(4, 0);

		assert "[x^2]".equals("" + a) : "" + a;
		assert "[3x]".equals("" + b) : "" + b;
		assert "[4]".equals("" + c) : "" + c;
	}

	public static void test2() {
		Poly a = new Poly(1, 2);
		Poly b = new Poly(3, 1);
		Poly ab = a.add(b);

		assert "[3x, x^2]".equals("" + ab);

		Poly f1 = new Poly(3, 5);
		Poly f2 = new Poly(3, 2);
		Poly f3 = new Poly(-2, 1);
		Poly f4 = new Poly(-4, 0);
		Poly f = f1.add(f2).add(f3).add(f4);
		assert "[-4, -2x, 3x^2, 3x^5]".equals("" + f);

		Poly g1 = new Poly(-4, 8);
		Poly g2 = new Poly(3, 4);
		Poly g3 = new Poly(-6, 2);
		Poly g4 = new Poly(5, 1);
		Poly g = g1.add(g2).add(g3).add(g4);
		assert "[5x, -6x^2, 3x^4, -4x^8]".equals("" + g);

		Poly fg = f.add(g);
		Poly gf = g.add(f);
		assert "[-4, 3x, -3x^2, 3x^4, 3x^5, -4x^8]".equals("" + fg);
		assert "[-4, 3x, -3x^2, 3x^4, 3x^5, -4x^8]".equals("" + gf);
	}

	public static void debug(String s) {
		System.out.println("dbg: " + s);
	}
}

class Term {
	public int coeff;

	public int pow;

	public Term(int coeff, int pow) {
		this.coeff = coeff;
		this.pow = pow;
	}

	public boolean equals(Object other) {
		if (other instanceof Term) {
			Term o = (Term) other;
			return this.pow == o.pow && this.coeff == o.coeff;
		} else {
			return false;
		}
	}

	public String toString() {
		if (coeff == 0) {
			return "0";
		} else if (pow == 0) {
			return "" + coeff;
		} else if (pow == 1) {
			return "" + (coeff != 1 ? coeff : "") + "x";
		} else {
			return "" + (coeff != 1 ? coeff : "") + "x^" + pow;
		}
	}
}
