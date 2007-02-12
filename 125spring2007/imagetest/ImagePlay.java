package imagetest;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Random;

import util.EasyInput;
import csimage.UberImage;
import csimage.show;

public class ImagePlay {

	public static void main(String[] args) {
		UberImage image = UberImage.fromFile(EasyInput.chooseFile());
		UberImage copy1 = new UberImage(image);
		UberImage copy2 = new UberImage(image);
		UberImage copy3 = new UberImage(image);
		UberImage copy4 = new UberImage(image);
		show.inFrame(image);
		highestValueRGB(copy1);
		show.inFrame(copy1);
		lowestValueRGB(copy2);
		show.inFrame(copy2);
		shuffleRGB(copy3);
		show.inFrame(copy3);
		sumModRGB(copy4);
		show.inFrame(copy4);
	}

	public static void makeBlackAndWhite(UberImage image) {
		makeBlackAndWhite(image, 3 * 255 / 2);
	}

	public static int brightness(Color c) {
		return c.getRed() + c.getGreen() + c.getBlue();
	}

	public static void makeBlackAndWhite(UberImage image, double rawThreshold) {
		assert 0 <= rawThreshold && rawThreshold <= 1;
		int threshold = (int) (3 * 255 * rawThreshold);

		for (int i = 0; i < image.getWidth(); ++i) {
			for (int j = 0; j < image.getHeight(); ++j) {
				Color c = image.getColor(i, j);
				if (brightness(c) < threshold) {
					image.setColor(i, j, Color.BLACK);
				} else {
					image.setColor(i, j, Color.WHITE);
				}
			}
		}
	}

	public static int maxRGB(Color c) {
		int r = c.getRed();
		int g = c.getGreen();
		int b = c.getBlue();
		return Math.max(Math.max(r, g), b);
	}

	public static Color minRGB(Color c) {
		int r = c.getRed();
		int g = c.getGreen();
		int b = c.getBlue();
		int smallest = Math.min(Math.min(r, g), b);
		if (r == smallest) {
			return new Color(r, 0, 0);
		} else if (g == smallest) {
			return new Color(0, g, 0);
		} else {
			return new Color(0, 0, b);
		}
	}

	public static Random rnd = new Random();

	public static Color randRGB(Color c) {
		int r = c.getRed();
		int g = c.getGreen();
		int b = c.getBlue();
		int rand = rnd.nextInt(3);
		if (rand == 0) {
			return new Color(r, 0, 0);
		} else if (rand == 1) {
			return new Color(0, g, 0);
		} else {
			return new Color(0, 0, b);
		}
	}

	public static void makeRandomRGB(UberImage image) {
		for (int i = 0; i < image.getWidth(); ++i) {
			for (int j = 0; j < image.getHeight(); ++j) {
				Color c = image.getColor(i, j);
				image.setColor(i, j, randRGB(c));
			}
		}
	}

	public static void shuffleRGB(UberImage image) {
		ArrayList<Integer> arr = new ArrayList<Integer>();
		arr.add(0);
		arr.add(0);
		arr.add(0);
		for (int i = 0; i < image.getWidth(); ++i) {
			for (int j = 0; j < image.getHeight(); ++j) {
				Color c = image.getColor(i, j);
				int r = c.getRed();
				int g = c.getGreen();
				int b = c.getBlue();
				arr.set(0, r);
				arr.set(1, g);
				arr.set(2, b);
				Collections.shuffle(arr);
				image.setColor(i, j, new Color(arr.get(0), arr.get(1), arr
						.get(2)));
			}
		}
	}

	public static void lowestValueRGB(UberImage image) {
		for (int i = 0; i < image.getWidth(); ++i) {
			for (int j = 0; j < image.getHeight(); ++j) {
				Color c = image.getColor(i, j);
				int r = c.getRed();
				int g = c.getGreen();
				int b = c.getBlue();
				int smallest = Math.min(Math.min(r, g), b);
				if (r == smallest) {
					image.setColor(i, j, new Color(r, 0, 0));
				} else if (g == smallest) {
					image.setColor(i, j, new Color(0, g, 0));
				} else {
					image.setColor(i, j, new Color(0, 0, b));
				}
			}
		}
	}

	public static void highestValueRGB(UberImage image) {
		for (int i = 0; i < image.getWidth(); ++i) {
			for (int j = 0; j < image.getHeight(); ++j) {
				Color c = image.getColor(i, j);
				int r = c.getRed();
				int g = c.getGreen();
				int b = c.getBlue();
				int highest = Math.max(Math.max(r, g), b);
				if (r == highest) {
					image.setColor(i, j, new Color(r, 0, 0));
				} else if (g == highest) {
					image.setColor(i, j, new Color(0, g, 0));
				} else {
					image.setColor(i, j, new Color(0, 0, b));
				}
			}
		}
	}

	public static void highestValueRGB3color(UberImage image) {
		for (int i = 0; i < image.getWidth(); ++i) {
			for (int j = 0; j < image.getHeight(); ++j) {
				Color c = image.getColor(i, j);
				int r = c.getRed();
				int g = c.getGreen();
				int b = c.getBlue();
				int highest = Math.max(Math.max(r, g), b);
				if (r == highest) {
					image.setColor(i, j, Color.RED);
				} else if (g == highest) {
					image.setColor(i, j, Color.GREEN);
				} else {
					image.setColor(i, j, Color.BLUE);
				}
			}
		}
	}

	public static void sumModRGB(UberImage image) {
		for (int i = 0; i < image.getWidth(); ++i) {
			for (int j = 0; j < image.getHeight(); ++j) {
				Color c = image.getColor(i, j);
				int r = c.getRed();
				int g = c.getGreen();
				int b = c.getBlue();
				int sm = (r + g + b) % 256;
				image.setColor(i, j, new Color(sm, sm, sm));
			}
		}
	}
}
