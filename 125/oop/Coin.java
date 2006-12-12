package oop;

public class Coin {

	private double headProb;

	public Coin() {
		this(0.5);
	}

	public Coin(double probOfHeads) {
		assert 0 <= probOfHeads && probOfHeads <= 1;
		this.headProb = probOfHeads;
	}

	public double headProb() {
		return this.headProb;
	}

	public double tailProb() {
		return 1 - this.headProb;
	}

	public Side flip() {
		if (Math.random() <= this.headProb) {
			return Side.heads;
		} else {
			return Side.tails;
		}
	}

}
