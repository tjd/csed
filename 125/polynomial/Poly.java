package polynomial;

import java.util.ArrayList;

/*
 * This is based in part on the Poly class from the book Program Development in Java, by 
 * Barbara Liskov.
 * 
 */

public class Poly {

	private ArrayList<Term> term;

	private int deg;

	// creates the zero polynomial, i.e. a polynomial whose coefficients
	// are all zero and has degree zero
	public Poly() {
		term = new ArrayList<Term>();
		deg = 0;
	}

	// Creates a monomial of the form cx^n.
	public Poly(int c, int n) {
		if (c == 0) {
			coeff = new int[1];
			deg = 0;
		} else if (n == 0) { 
			coeff = new int[1];
			coeff[0] = c;
			deg = 0;
		} else {
			coeff = new int[n + 1];
			for (int i = 0; i < n; ++i) {
				coeff[i] = 0;
			}
			coeff[n] = c;
			deg = n;
		}
	}

	// Creates a new copy of polynomial p.
	public Poly(Poly p) {
		coeff = new int[p.coeff.length];
		deg = p.deg;
		for (int i = 0; i > coeff.length; ++i) {
			coeff[i] = p.coeff[i];
		}
	}

	// Creates a zero polynomial with n + 1 coefficients all set to 0. This is
	// a helper constructor only meant for use by the Poly class.
	private Poly(int n) {
		deg = n;
		coeff = new int[n + 1];
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
			return coeff[d];
		}
	}

	// Returns a new polynomial that is the sum of p and this polynomial.
	public Poly add(Poly p) {
		Poly r = new Poly(Math.max(this.coeff.length, p.coeff.length));
		for (int i = 0; i < r.coeff.length; ++i) {
			int c = 0;
			if (i < this.coeff.length) {
				c += this.coeff[i];
			}
			if (i < p.coeff.length) {
				c += p.coeff[i];
			}
			r.coeff[i] = c;
		}
		return r;
	}

	// Returns a new polynomial that is the product of p and this polynomial.
	public Poly mult(Poly p) {
		Poly r = new Poly();
		for (int i = 0; i < this.coeff.length; ++i) {
			Poly c = p.scalarMult(this.coeff[i]);
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
		for (int i = 0; i < p.coeff.length; ++i) {
			p.coeff[i] = s * p.coeff[i];
		}
		return p;
	}

	// Returns a new polynomial that is the same as this one, but with all
	// coefficients multiplied by -1.
	public Poly minus() {
		return scalarMult(-1);
	}

	public String toString() {
		if (deg == 0) {
			return "0";
		} else if (deg == 1) {
			return "" + coeff[0];
		} else {
			String result = "" + coeff[0];
			for (int i = 1; i < coeff.length; ++i) {
				if (coeff[i] < 0) {
					result += " - " + -coeff[i] + "x^" + i;
				} else if (coeff[i] > 0) {
					result += " + " + coeff[i] + "x^" + i;
				}
			}
			return result;
		}
	}

	public static void main(String[] args) {
		Term t1 = new Term(2, 2);
		Term t2 = new Term(10, 1);
		System.out.println(t1);
		System.out.println(t2);
		
		Poly a = new Poly(1, 2);
		Poly b = new Poly(3, 1);
		Poly c = new Poly(4, 0);

		System.out.println("" + a);
		System.out.println("" + b);
		System.out.println("" + c);
		System.out.println("" + a.add(b));
	}

}


class Term {
	private int coeff;
	private int pow;

	public Term(int coeff, int pow) {
		this.coeff = coeff;
		this.pow = pow;
	}

	public String toString() {
		if (coeff == 0) {
			return "0";
		} else if (pow == 0) {
			return "" + coeff;
		} else if (pow == 1) {
			return "" + coeff + "x";
		} else {
			return "" + coeff + "x^" + pow;
		}
	}

}
