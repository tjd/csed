package csimage.demo;

import csimage.UberImage;
import csimage.show;
import csimage.util.EasyInput;

public class ViewImage {

	public static void main(String[] args) {
		String path = EasyInput.chooseFile();
		UberImage pic = UberImage.fromFile(path);
		show.inFrame(pic);
	}
}
