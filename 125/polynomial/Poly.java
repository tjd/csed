package polynomial;

import java.util.ArrayList;

/*
 * This is based in part on the Poly class from the book Program Development in Java, by 
 * Barbara Liskov.
 * 
 */

public class Poly {

	private ArrayList<Term> terms;

	// creates the zero polynomial, i.e. a polynomial whose coefficients
	// are all zero and has degree zero
	public Poly() {
		terms = new ArrayList<Term>();
	}

	// Creates a monomial of the form cx^n.
	public Poly(int c, int n) {
		terms = new ArrayList<Term>();
		terms.add(new Term(c, n));
	}

	// Creates a new copy of polynomial p.
	public Poly(Poly p) {
		terms = new ArrayList<Term>();
		for (Term t : p.terms) {
			terms.add(t);
		}
	}

	// Returns the degree of this polynomial, i.e. the largest exponent
	// with a non-zero coefficient. Returns 0 for the zero polynomial.
	public int degree() {
		if (terms.size() == 0) {
			return 0;
		} else {
			return terms.get(terms.size() - 1).pow;
		}
	}

	// Returns the coefficient of the term with exponent d.
	public int coeff(int d) {
		if (d < 0 || d > degree()) {
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
				result.terms.add(p.terms.get(b));
				++b;
			} else if (b >= B) {
				result.terms.add(this.terms.get(a));
				++a;
			} else {
				Term termA = this.terms.get(a);
				Term termB = p.terms.get(b);
				if (termA.pow == termB.pow) {
					result.terms.add(new Term(termA.coeff + termB.coeff,
							termA.pow));
					++a;
					++b;
				} else if (termA.pow < termB.pow) {
					result.terms.add(termA);
					++a;
				} else if (termA.pow > termB.pow) {
					result.terms.add(termB);
					++b;
				} else {
					throw new RuntimeException("something impossible has happened!");
				}
			}
		}
		return result;
	}

	// Returns a new polynomial that is the same as this one, but each
	// term has been multiplied by t.
	public Poly termMult(Term t) {
		Poly result = new Poly();
		for (Term m : terms) {
			result.terms.add(m.mult(t));
		}
		return result;
	}

	// Returns a new polynomial that is the product of p and this polynomial.
	public Poly mult(Poly p) {
		Poly r = new Poly();
		for (Term t : terms) {
			r = r.add(p.termMult(t));
		}
		return r;
	}

	// Returns a new polynomial that is the difference of p and this
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

	// Evaluates this polynomial by setting the variable x to be the given
	// value.
	public int eval(int x) {
		int result = 0;
		for (Term t : terms) {
			result += t.eval(x);
		}
		return result;
	}

	public String toString() {
		return "" + terms;
	}

	public static void main(String[] args) {
		test1();
		test2();
		test3();
		test4();
		System.out.println("All tests passed!");
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
		assert a.degree() == 2;
		assert b.degree() == 1;
		assert ab.degree() == 2;

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

	public static void test3() {
		Poly a = new Poly(1, 2);
		Poly b = new Poly(3, 1);
		Poly ab = a.mult(b);
		assert "[3x^3]".equals("" + ab);
		assert ab.degree() == 3;

		Poly aa = a.mult(a);
		assert "[x^4]".equals("" + aa);
		assert aa.degree() == 4;

		Poly f1 = new Poly(1, 1);
		Poly f2 = new Poly(1, 0);
		Poly f = f1.add(f2);

		assert "[1, x]".equals("" + f);
		Poly ff = f.mult(f);
		assert "[1, 2x, x^2]".equals("" + ff);
	}

	public static void test4() {
		Poly a = new Poly(1, 2);
		Poly b = new Poly(3, 1);
		Poly ab = a.mult(b);
		assert "[3x^3]".equals("" + ab);
		assert a.eval(0) == 0;
		assert a.eval(1) == 1;
		assert b.eval(2) == 6;
		assert b.eval(3) == 9;
		assert ab.eval(4) == (3 * 4 * 4 * 4);
		assert ab.eval(5) == (3 * 5 * 5 * 5);
	}

	public static void debug(String s) {
		System.out.println("dbg: " + s);
	}
}

class Term {
	public final int coeff;

	public final int pow;

	public Term(int coeff, int pow) {
		this.coeff = coeff;
		this.pow = pow;
		selfCheck();
	}

	public boolean equals(Object x) {
		if (x instanceof Term) {
			Term other = (Term) x;
			return this.pow == other.pow && this.coeff == other.coeff;
		} else {
			return false;
		}
	}

	public Term mult(Term t) {
		return new Term(this.coeff * t.coeff, this.pow + t.pow);
	}

	public int eval(int x) {
		return (int) (coeff * Math.pow(x, pow));
	}

	public String toString() {
		if (coeff == 0) {
			return "0";
		} else if (pow == 0) {
			return "" + coeff;
		} else {
			return String.format("%sx%s", (coeff != 1 ? coeff : ""),
					(pow == 1 ? "" : ("^" + pow)));
		}
		// } else if (pow == 1) {
		// return "" + (coeff != 1 ? coeff : "") + "x";
		// } else {
		// return "" + (coeff != 1 ? coeff : "") + "x^" + pow;
		// }
	}

	public void selfCheck() {
		if (pow < 0) {
			throw new RuntimeException("Term inconsistent: " + this
					+ "\ncoeff >= 0 required");
		}
	}
}
