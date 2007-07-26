package csimage.demo.imageprocessing;

import java.awt.Color;
import java.util.Date;
import java.util.HashMap;

import csimage.UberImage;
import csimage.util.EasyInput;

public class ColorCount2 {

	public static void main(String[] args) {
		csimage.UberColor C1 = new csimage.UberColor(194, 157, 142);
		csimage.UberColor C2 = new csimage.UberColor(220, 193, 180);
		//String fname = EasyInput.chooseFile();
		String fname = "/Users/toby/Desktop/scratch/aidan2lego.png";
		UberImage img = UberImage.fromFile(fname);
		HashMap<Color, Integer> map = new HashMap<Color, Integer>();

		System.out.println(new Date());
		for (int x = 0; x < 440; x++) {
			for (int y = 0; y < 440; y++) {
				Color c = img.getColor(x, y);
				if (map.containsKey(c)) {
					map.put(c, map.get(c) + 1);
				} else {
					map.put(c, 1);
				}
				if (c.equals(C1)) {
					img.setColor(x, y, C2);
				} else if (c.equals(C2)){
					img.setColor(x, y, C1);
				}
			}
		}
		
		csimage.show.inFrame(img);
		
		System.out.println(new Date());
		System.out.printf("Width = %s pixels", img.getWidth());
		System.out.printf("Height = %s pixels", img.getHeight());
		System.out.printf("%s has exactly %s different colors\n", fname, map
				.size());
		for (Color c : map.keySet()) {
			System.out.printf("Color %s : %s (%s)\n", c, map.get(c), map.get(c) / 100);
		}
		
		System.out.printf("Color at (20, 20) = " + img.getColor(20, 20));
	}
}