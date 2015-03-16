Instructions for using the UberImage class.

# Introduction #

To start using UberImage, you must first install it. AddingCsImageToEclipse explains how to download and install the necessary library file.


# UberImage Method Summary #

The following summarizes the most useful methods in UberImage. Note that the first three methods are static methods, and they are the usual way for creating new UberImages.

  * **UberImage.fromFile(String name)**: returns an UberImage object that is a copy of the image stored in the given file. The file must be an image of type PNG, JPEG, BMP, or GIF (and possibly others --- it can read whatever image formats the Java ImageIO library supports).
  * **UberImage fromURL(URL url)**: returns a new UberImage object that is a copy of the image at the given URL.
  * **UberImage.blankImage(int width, int height, Color color)**: returns a new UberImage object that is width pixels wide and height pixels high, with each pixel set to the given color.
  * **getWidth()**: returns the width of the image in pixels.
  * **getHeight()**: returns the height of the image in pixels.
  * **setColor(int x, int y, Color c)**: sets the pixel at location (x, y) to be the given color. Note that the upper left hand of the corner is (0, 0), and the x-values increase to the right, and the y-values decrease down (so the y-values go in the opposite direction of regular mathematical coordinates).
  * **getColor(int x, int y)**: returns the color of the pixel at (x, y) as a Color object.
  * **saveAsJPEG(String name)**: saves the current image in a file with the given name in the JPEG format. JPEG compresses images quite a bit, and so it's a popular format for storing photographic images. The downside is some details are lost in the compression process, which results in minor visual imperfections, such as smearing or blurring of details. Thus, JPEG is not a good format for storing images (such as icons) where the pixel colors must be exact.
  * **saveAsPNG(String name)**: saves the current image in a file with the given name in the PNG format. PNG files save the pixel colors exactly, although they tend to be bigger than equivalent JPEG files.

UberImage has other methods, but these ones are the main ones. It's worth noting that UberImage is a subclass of Java's BufferedImage class, and so any code that needs a BufferedImage can use an UberImage instead.

The UberColor class is designed to work with the UberImage class, and it is a subclass of the Java Color class. In addition to doing everything Color object do, UberColor objects provide helper methods, such as lightness() and luminance().

# Sample Code #
Here is an example of how to use UberImage to create a simple image viewer:

```
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
```

The `csimage.show` method is a simple helper class for quick and dirty display of images in frames.