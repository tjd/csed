How to add the [csimage.jar](http://csed.googlecode.com/files/csimage.jar) file to an Eclipse project.

  1. Download [csimage.jar](http://csed.googlecode.com/files/csimage.jar) to your computer.
  1. Launch Eclipse and right-click on a Java project. Select "Build Path" and then "Add External Archives...". In the box that pops up, select the [csimage.jar](http://csed.googlecode.com/files/csimage.jar) file you just downloaded.
  1. Now [csimage.jar](http://csed.googlecode.com/files/csimage.jar) should be part of your project, and you can use any of the classes it provides.

To test that you've properly installed [csimage.jar](http://csed.googlecode.com/files/csimage.jar), enter this sample program:

```
import csimage.UberImage;
import csimage.show;
import csimage.util.EasyInput;

public class TestImage {

	public static void main(String[] args) {
		String fname = EasyInput.chooseFile();
		UberImage img = UberImage.fromFile(fname);
		show.inFrame(img);
	}
}
```

See UsingUberImage for documentation of its most commonly used methods.