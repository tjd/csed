package csimage.demo.imageprocessing;

import java.awt.Color;

import csimage.UberColor;
import csimage.UberImage;
import csimage.show;
import csimage.util.EasyInput;

public class CopyImage {

	public static void main(String[] args) {
		EasyInput.printCwd();
		UberImage original = UberImage.fromFile("csimage/pictures/sumo.jpg");
		show.inFrame(original);

		UberImage copy = UberImage.fromImage(original);

		copy.fillSquare(100, 100, 25, new UberColor(Color.YELLOW));
		show.inFrame(copy);
	}
}
