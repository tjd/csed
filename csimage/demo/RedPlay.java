/*
 * Created on Jan 10, 2005 Converts a normal colour image to one that is all
 * shades of one colour. By varying the parameters the level of filtering can be
 * adjusted.
 */
package csimage.demo;

import java.awt.Color;

import csimage.UberImage;
import csimage.show;

public class RedPlay {

	public static void main(String[] args) {
		// Show the color image for comparison purposes
		UberImage m1 = UberImage.fromFile("csimage/pictures/sumo.jpg");
		show.inFrame(m1);

		UberImage m2 = UberImage.fromFile("csimage/pictures/sumo.jpg");
		shadesOfRed(m2, 10.0);
		show.inFrame(m2);
	}

	/*
	 * The double parameter determines what levels of green and blue are
	 * retained. A value of 1.0 is unchanaged and a value of 255.0 completely
	 * removes G and B. From tests anything over 3.0 or 4.0 results in red
	 * predominating
	 */
	public static void shadesOfRed(UberImage m, double strength) {
		for (int i = 0; i < m.getWidth(); ++i) {
			for (int j = 0; j < m.getHeight(); ++j) {
				Color oldColor = m.getColor(i, j);
				int newRed = oldColor.getRed();
				int newGreen = (int) (oldColor.getGreen() / strength);
				int newBlue = (int) (oldColor.getBlue() / strength);
				Color newColor = new Color(newRed, newGreen, newBlue);
				m.setColor(i, j, newColor);
			}
		}
	}
}
