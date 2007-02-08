package patterns;

import java.awt.Color;

import csimage.UberImage;
import csimage.show;

public class PrimePattern {

	public static boolean isPrime(int n) {
		if (n <= 1) {
			return false;
		} else if (n == 2) {
			return true;
		} else if (n % 2 == 0) {
			return false;
		} else {
			int d = 3;
			while (d * d <= n) {
				if (n % d == 0) {
					return false;
				}
				d = d + 2;
			}
			return true;
		}
	}

	public static void main(String[] args) {
		UberImage img = UberImage.blankImage(500, 500);
		for (int i = 0; i < img.getWidth(); ++i) {
			for (int j = 0; j < img.getHeight(); ++j) {
				int num = i * img.getWidth() + j + 1;
				if (isPrime(num)) {
					img.setColor(i, j, Color.BLACK);
				} else {
					img.setColor(i, j, Color.WHITE);
				}
			}
		}
		show.inFrame(img);
	}

}
