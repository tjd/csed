package misc;

import java.awt.Color;
import java.util.ArrayList;

public class Tree {

	private ArrayList<Leaf> leaves;

	private Color color;

	public Tree(int n, Color c) {
		leaves = new ArrayList<Leaf>();
		color = c;
		for (int i = 0; i < n; ++i) {
			leaves.add(new Leaf(color));
		}
	}

	public Color getColor() {
		return color;
	}

	public int getNumLeaves() {
		return leaves.size();
	}

	public void shedHalf() {
		int n = getNumLeaves();
		if (n > 0) {
			for (int i = 0; i < n / 2; ++i) {
				leaves.remove(0);
			}
		}
	}

	public static void main(String[] args) {
		Tree tree = new Tree(30, Color.RED);
		System.out.println("number of leaves: " + tree.getNumLeaves());
		tree.shedHalf();
		System.out.println("number of leaves: " + tree.getNumLeaves());
		tree.shedHalf();
		System.out.println("number of leaves: " + tree.getNumLeaves());
	}

}
