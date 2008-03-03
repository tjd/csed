package imagetest;

import util.EasyInput;
import csimage.UberImage;
import csimage.show;

public class ImageViewer {

	public static void main(String[] args) {
		String fname = EasyInput.chooseFile();
		UberImage img = UberImage.fromFile(fname);
		show.inFrame(img);
	}

}
