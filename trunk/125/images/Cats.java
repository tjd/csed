package images;

/*
 * Script to crop the image of Newton.
 */

import java.util.Arrays;

import csimage.UberImage;
import csimage.util.EasyInput;

public class Cats {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		String IMAGE_DIR = "/Users/toby/Desktop/spring 2006/125/images/";
		System.out.printf("IMAGE_DIR = %s\nimages: %s\n", IMAGE_DIR, Arrays
				.toString(EasyInput.listdir(IMAGE_DIR)));

		UberImage newton = UberImage.fromFile(IMAGE_DIR + "newton.jpg");
		UberImage drnebulon = UberImage.fromFile(IMAGE_DIR + "drnebulon.jpg");
		System.out.printf("newton: %s x %s\n", newton.getWidth(), newton
				.getHeight());
		System.out.printf("drnebulon: %s x %s", drnebulon.getWidth(), drnebulon
				.getHeight());

		UberImage croppedNewton = UberImage.blankImage(422, 500);
		for (int i = 0; i < croppedNewton.getWidth(); ++i) {
			for (int j = 0; j < croppedNewton.getHeight(); ++j) {
				croppedNewton.setColor(i, j, newton.getColor(i, j));
			}
		}

		croppedNewton.saveAsJPEG(IMAGE_DIR + "croppedNewton.jpg");
	}

}
