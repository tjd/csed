package csimage.demo;

import csimage.UberColor;
import csimage.UberImage;
import csimage.show;

public class RGBplay {

	public static void main(String[] args) {
		// show the color image for comparison purposes
		UberImage m1 = UberImage.fromFile("csimage/pictures/sumo.jpg");
		show.inFrame(m1);

		UberImage m2 = UberImage.fromFile("csimage/pictures/sumo.jpg");

		// play with RGB values of m2
		for (int i = 0; i < m2.getWidth(); i++) {
			for (int j = 0; j < m2.getHeight(); j++) {
				UberColor c = m2.getColor(i, j);
				m2.setColor(i, j, c.getBlue(), c.getGreen(), c.getRed());
			}
		}
		show.inFrame(m2);
	}
}