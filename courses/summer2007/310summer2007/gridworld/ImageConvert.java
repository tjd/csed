package gridworld;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;

import csimage.UberColor;
import csimage.UberImage;

public class ImageConvert {

	public static void makeTextFile(String imagefilename, String outfilename) {
		try {
			UberImage img = UberImage.fromFile(imagefilename);
			OutputStreamWriter out = new OutputStreamWriter(
					new FileOutputStream(outfilename));
			for (int r = 0; r < img.getHeight(); ++r) {
				for (int c = 0; c < img.getWidth(); ++c) {
					UberColor uc = img.getColor(c, r);
					out.write((uc.dist(UberColor.WHITE) < uc.dist(UberColor.BLACK)) ? "." : "w");
				}
				out.write("\n");
			}
			out.close();
			System.out.printf("Image \"%s\" written to text file \"%s\".", imagefilename, outfilename);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public static void main(String[] args) {
        makeTextFile("gohome.png", "mazeout.txt");
	}

}
