package csimage.demo.wolfram;

import java.awt.Color;

import csimage.UberImage;
import csimage.show;

public class WolframAutomata3 {
	public static void main(String[] args) {
		drawWolfram((byte) 30);
	}

	public static void drawWolfram(byte rule) {
		UberImage m = UberImage.blankImage(800, 800, Color.white);

		// Set the first pixel
		m.setColor(m.getWidth() / 2, 0, Color.black);

		byte lastThree;
		// Loop through the rows
		for (int y = 1; y < m.getHeight(); y++) {
			// Loop through each pixel in the row
			for (int x = 0; x < m.getWidth(); x++) {
				// Get the code of the previous three pixels
				lastThree = 0;
				if (((x + 1) < m.getWidth())
						&& (m.getColor(x + 1, y - 1).equals(Color.black)))
					lastThree |= 1;
				if (m.getColor(x, y - 1).equals(Color.black))
					lastThree |= 2;
				if ((x > 0) && m.getColor(x - 1, y - 1).equals(Color.black))
					lastThree |= 4;

				// Now determine the value of the current pixel and set it
				if (((rule >> lastThree) & 1) == 1)
					m.setColor(x, y, Color.black);
			}
		}
		show.inFrame(m);
	}

}