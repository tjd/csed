package csimage.demo.wolfram;

import java.awt.Color;

import csimage.UberImage;
import csimage.show;

/**
 * 
 * A Wolfram 1D automata, made famous in his book A New Kind of Science.
 * 
 */
public class WolframAutomata {

	public static void main(String[] args) {
		UberImage m = UberImage.blankImage(500, 500, Color.WHITE);

		// initialize the first row
		m.setColor(250, 0, Color.BLACK);

		for (int y = 1; y < m.getHeight(); y++) {
			for (int x = 1; x < m.getWidth() - 1; x++) {
				boolean left = m.getColor(x - 1, y - 1).equals(Color.BLACK);
				boolean mid = m.getColor(x, y - 1).equals(Color.BLACK);
				boolean right = m.getColor(x + 1, y - 1).equals(Color.BLACK);
				if (x == 250 && y == 1) {
					System.out.println("" + left + " " + mid + " " + right);
					System.out.println(m.getColor(x, y - 1));
					m.setColor(x, y, Color.RED);
				}
				if (left && mid && right) {
				} else if (left && mid && !right) {
				} else if (left && !mid && right) {
				} else if (left && !mid && !right) {
					m.setColor(x, y, Color.BLACK);
				} else if (!left && mid && right) {
					m.setColor(x, y, Color.BLACK);
				} else if (!left && mid && !right) {
					m.setColor(x, y, Color.BLACK);
				} else if (!left && !mid && right) {
					m.setColor(x, y, Color.BLACK);
				} else if (!left && !mid && !right) {
				} else {
					System.out.print("Error! Error!");
				}
			}
		}

		show.inFrame(m);
		m.saveAsPNG("wolfram.png");
		System.out.println("done");
	}
}