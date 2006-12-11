package csimage.demo.imageprocessing;

import java.util.Date;
import java.util.HashSet;

import csimage.UberImage;
import csimage.util.EasyInput;

public class ColorCount {

	public static void main(String[] args) {
		String fname = EasyInput.chooseFile();
		UberImage img = UberImage.fromFile(fname);
		HashSet<Integer> set = new HashSet<Integer>();

		System.out.println(new Date());
		for (int x = 0; x < img.getWidth(); x++) {
			for (int y = 0; y < img.getHeight(); y++) {
				// Color c = img.getColor(x, y);
				set.add(img.getRGB(x, y));
			}
		}
		System.out.println(new Date());
		System.out.printf("%s has exactly %s different colors", fname, set.size());
		System.out.printf("Width = %s pixels", img.getWidth());
		System.out.printf("Height = %s pixels", img.getHeight());
	}
}