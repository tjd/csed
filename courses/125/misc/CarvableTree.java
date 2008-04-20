package misc;

import java.awt.Color;

public class CarvableTree extends Tree {

	private String letters;

	public CarvableTree(int numLeaves, Color color) {
		super(numLeaves, color);
	}

	public String getLetters() {
		return letters;
	}

	public void setLetters(String letters) {
		this.letters = letters;
	}

	public static void main(String[] args) {
		CarvableTree tree = new CarvableTree(55, Color.YELLOW);
		tree.setLetters("YC & JD");
		System.out
				.printf(
						"This tree has\n%s %s leaves, with \"%s\" carved into the bark",
						tree.getNumLeaves(), tree.getColor(), tree.getLetters());
	}

}
