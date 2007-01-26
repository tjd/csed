package imagetest;

import csimage.UberImage;
import csimage.show;
import util.EasyInput;

public class ImageViewer {

	public static void main(String[] args) {
		String fname = EasyInput.chooseFile();
		UberImage img = UberImage.fromFile(fname);
		show.inFrame(img);
	}

}
