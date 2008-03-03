package imagetest;

import java.awt.Color;

import csimage.UberImage;
import csimage.show;

public class Imagetest {

	public static void main(String[] args) {
		UberImage img = UberImage.fromFile("C:\\Documents and Settings\\tjd\\Desktop\\125\\boy.jpg");
		show.inFrame(img);
		makeBlackAndWhite(img);
		show.inFrame(img);
	}

	public static void makeBlackAndWhite(UberImage img) {
		for(int i = 0; i < img.getWidth(); ++i) {
			for(int j = 0; j < img.getHeight(); ++j) {
				Color c = img.getColor(i, j);
				int brightness = c.getRed() + c.getGreen() + c.getBlue();
				final int THRESHOLD = (255 * 3)/2;
				if (brightness > THRESHOLD) {
					img.setColor(i, j, Color.BLACK);
				} else {
					img.setColor(i, j, Color.WHITE);
				}
			}
		}
	}
	
}
