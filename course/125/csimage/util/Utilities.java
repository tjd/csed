package csimage.util;

/**
 * Various image utilities. Originally based on Jonathan Knudsen's
 * ApplicationFrame class, from his O'Reilly book Java 2D Graphics. comments
 * edited: Jan 27, 2005
 */

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Insets;
import java.awt.MediaTracker;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URL;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;

import com.sun.image.codec.jpeg.JPEGCodec;
import com.sun.image.codec.jpeg.JPEGEncodeParam;
import com.sun.image.codec.jpeg.JPEGImageDecoder;
import com.sun.image.codec.jpeg.JPEGImageEncoder;

public class Utilities extends dbg {

	public static final float DEFAULT_JPEG_QUALITY = 0.85f;

	private static final Component sComponent = new Component() {
	};

	private static final MediaTracker sTracker = new MediaTracker(sComponent);

	private static int sID = 0;

	public static boolean waitForImage(Image image) {
		int id;
		synchronized (sComponent) {
			id = sID++;
		}
		sTracker.addImage(image, id);
		try {
			sTracker.waitForID(id);
		} catch (InterruptedException ie) {
			return false;
		}
		if (sTracker.isErrorID(id))
			return false;
		return true;
	}

	/**
	 * Returns a loaded image from the given path.
	 * 
	 * @param path
	 *            (fully qualified) name of where to load the image
	 * @return a loaded image
	 */
	public static Image loadImageFromFile(String path) {
		ImageIcon ii = new ImageIcon(path);
		assert ii != null;
		Image img = ii.getImage();
		assert img != null;
		return img;
	}

	/**
	 * Returns a loaded BufferedImage from the given path.
	 * 
	 * @param path
	 *            (fully qualified) name of where to load the image
	 * @return a loaded BufferedImage
	 */
	public static BufferedImage loadBufferedImageFromFile(String path) {
		return makeBufferedImage(loadImageFromFile(path));
	}

	public static Image blockingLoad(String path) {
		Image image = Toolkit.getDefaultToolkit().getImage(path);
		if (waitForImage(image) == false)
			return null;
		return image;
	}

	public static Image blockingLoad(URL url) {
		Image image = Toolkit.getDefaultToolkit().getImage(url);
		if (waitForImage(image) == false)
			return null;
		return image;
	}

	public static BufferedImage makeBufferedImage(Image image) {
		return makeBufferedImage(image, BufferedImage.TYPE_INT_RGB);
	}

	public static BufferedImage makeBufferedImage(Image image, int imageType) {
		if (waitForImage(image) == false)
			return null;

		BufferedImage bufferedImage = new BufferedImage(image.getWidth(null),
				image.getHeight(null), imageType);
		Graphics2D g2 = bufferedImage.createGraphics();
		g2.drawImage(image, null, null);
		return bufferedImage;
	}

	public static Frame getNonClearingFrame(String name, Component c) {
		final Frame f = new Frame(name) {
			public void update(Graphics g) {
				paint(g);
			}
		};
		sizeContainerToComponent(f, c);
		centerFrame(f);
		f.setLayout(new BorderLayout());
		f.add(c, BorderLayout.CENTER);
		f.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				f.dispose();
			}
		});
		return f;
	}

	/**
	 * Resizes the container to the size of the component
	 * 
	 * @param container
	 * @param component
	 */
	public static void sizeContainerToComponent(Container container,
			Component component) {
		if (container.isDisplayable() == false)
			container.addNotify();
		Insets insets = container.getInsets();
		Dimension size = component.getPreferredSize();
		int width = insets.left + insets.right + size.width;
		int height = insets.top + insets.bottom + size.height;
		container.setSize(width, height);
	}

	/**
	 * Sets the frame f to appear in the middle of the screen.
	 * 
	 * @param f
	 */
	public static void centerFrame(Frame f) {
		Dimension screen = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension d = f.getSize();
		final int x = (screen.width - d.width) / 2;
		final int y = (screen.height - d.height) / 2;
		f.setLocation(x, y);
	}

	/**
	 * Changes the size of the given container so that it will correctly contain
	 * the given image.
	 * 
	 * @param container
	 * @param image
	 */
	public static void resizeContainer(Container container, Image image) {
		resizeContainer(container, image.getWidth(container), image
				.getHeight(container));
	}

	/**
	 * Changes the size of the given container so that it can properly contain
	 * the given image at the given scaling factor.
	 * 
	 * @param container
	 * @param image
	 * @param scaleFactor
	 *            the scaling factor
	 */
	public static void resizeContainer(Container container, Image image,
			double scaleFactor) {
		assert image != null;
		assert container != null;
		int width = (int) (image.getWidth(container) * scaleFactor);
		int height = (int) (image.getHeight(container) * scaleFactor);
		resizeContainer(container, width, height);
	}

	/**
	 * Changes the size of the container by the given width and height.
	 * 
	 * @param container
	 * @param width
	 *            new width of the container
	 * @param height
	 *            new height of the container
	 */
	public static void resizeContainer(Container container, int width,
			int height) {
		if (!container.isDisplayable()) {
			container.addNotify();
		}
		Insets inset = container.getInsets();
		int cwidth = inset.left + inset.right + width;
		int cheight = inset.top + inset.bottom + height;
		container.setSize(cwidth, cheight);
	}

	/**
	 * Returns true iff if path.toLowerCase() ends with ".jpg" or ".jpeg".
	 */
	public static boolean isJpeg(String path) {
		String name = path.toLowerCase();
		return name.endsWith(".jpg") || name.endsWith(".jpeg");
	}

	/**
	 * Returns true iff f is a JPEG file, according to its extension.
	 */
	public static boolean isJpeg(File f) {
		return isJpeg(f.getName());
	}

	/**
	 * Returns true iff if path.toLowerCase() ends with ".png".
	 */
	public static boolean isPng(String path) {
		String name = path.toLowerCase();
		return name.endsWith(".png");
	}

	/**
	 * Returns true iff f is a PNG file, according to its extension.
	 */
	public static boolean isPng(File f) {
		return isPng(f.getName());
	}

	/**
	 * Returns true iff if path.toLowerCase() ends with ".gif".
	 */
	public static boolean isGif(String path) {
		String name = path.toLowerCase();
		return name.endsWith(".gif");
	}

	/**
	 * Returns true iff f is a GIF file, according to its extension.
	 */
	public static boolean isGif(File f) {
		return isGif(f.getName());
	}

	/**
	 * Loads an image from the given path using javax.imageio.ImageIO image file
	 * type: jpg, png, gif, bmp, added as an alternative of
	 * loadBufferedImageFromFile(String path)
	 * 
	 * @param path
	 *            (fully qualified) name of where to load the image
	 * @return a loaded BufferedImage See <a
	 *         href="http://www.jguru.com/faq/view.jsp?EID=730341">Reference
	 *         from www.jguru.com </a>
	 */
	public static BufferedImage getBufferedImageFromFile(String path) {
		try {
			File inputFile = new File(path);
			String name = inputFile.getName();
			assert isJpeg(name) || isPng(name) || isGif(name) : "file name ends with .jpg, .jpeg, .png, or .gif";

			BufferedImage image = ImageIO.read(inputFile);
			if (waitForImage(image) == false) {
				dbg.sayln("... matrix " + path + " not loaded: ");
				return null;
			}

			// dbg.sayln("... matrix " + path + " loaded");
			return image;
		} catch (IOException e) {
			throw new Error("getBufferedImageFromFile failed due to "
					+ "an IOException while trying to open " + quote(path)
					+ ".");
		}
	}

	/**
	 * Loads a JPEG image from the given path
	 * 
	 * @param path
	 *            (fully qualified) name of where to load the image
	 * @return a loaded BufferedImage See <a
	 *         href="http://www.nsftools.com/tips/JavaTips.htm#jpgimage">Reference
	 *         </a>
	 */
	public static BufferedImage getBufferedImageFromJPEGFile(String path) {
		try {
			// File inputFile = new File(path);
			assert isJpeg(path) : "file name ends with .jpg or .jpeg";

			FileInputStream fis = new FileInputStream(path);
			JPEGImageDecoder decoder = JPEGCodec.createJPEGDecoder(fis);
			BufferedImage image = decoder.decodeAsBufferedImage();
			if (waitForImage(image) == false) {
				dbg.sayln("... matrix " + path + " not loaded: ");
				return null;
			}

			dbg.sayln("... matrix " + path + " loaded");
			return image;
		} catch (Exception e) {
			throw new Error("Error in getBufferedImageFromJPEGFile");
		}
	}

	/**
	 * Saves a JPEG image to the given path
	 * 
	 * @param image
	 *            an image
	 * @param path
	 *            (fully qualified) name of where to save the image
	 */
	public static void saveBufferedImageAsJPEG(BufferedImage image, String path) {
		saveBufferedImageAsJPEG(image, DEFAULT_JPEG_QUALITY, path);
	}

	/**
	 * Saves a JPEG image to the given path
	 * 
	 * @param image
	 *            an image
	 * @param quality
	 *            0.0 to 1.0; the higher the quality, the better the resolution
	 *            and the larger the file, e.g. a 500x500 mandelbrot image is
	 *            size 16.3KB for quality 0.75f, and 56.2KB for quality 1.0f.
	 * @param path
	 *            (fully qualified) name of where to save the image See
	 *            Reference <a
	 *            href="http://www.jguru.com/faq/view.jsp?EID=242020">one </a>
	 *            and <a
	 *            href="http://www.developer.com/java/other/article.php/606541">two
	 *            </a> uses com.sun.image.codec.jpeg.*;
	 */
	public static void saveBufferedImageAsJPEG(BufferedImage image,
			float quality, String path) {
		try {
			OutputStream out = new FileOutputStream(path);
			assert isJpeg(path) : "file name ends with .jpg or .jpeg";

			JPEGImageEncoder encoder = JPEGCodec.createJPEGEncoder(out);
			JPEGEncodeParam param = encoder.getDefaultJPEGEncodeParam(image);
			param.setQuality(quality, true);
			encoder.encode(image, param);
			out.close();
			// dbg.sayln("... matrix " + path + " saved");
		} catch (Exception e) {
			throw new Error("Error in saveBufferedImageAsJPEG");
		}
	}

	/**
	 * Saves a typed image according to the name of the given path to the path
	 * 
	 * @param image
	 *            an image
	 * @param path
	 *            (fully qualified) name of where to save the image
	 */
	public static void saveBufferedImageToTypedFile(BufferedImage image,
			String path) {
		String name = path.toLowerCase();
		if (isJpeg(name)) {
			saveBufferedImageToTypedFile(image, "JPEG", path);
		} else if (isPng(name)) {
			saveBufferedImageToTypedFile(image, "PNG", path);
		} else {
			throw new Error("... only extension \".jpg\", \".jpeg\""
					+ " and \".png\" are supported");
		}
	}

	/**
	 * Saves a JPEG or PNG image to the given path
	 * 
	 * @param image
	 *            an image
	 * @param fileType
	 *            the type of an image to be saved
	 * @param path
	 *            (fully qualified) name of where to save the image See <a
	 *            href="http://www.jguru.com/faq/view.jsp?EID=730341">Reference
	 *            </a>
	 */
	public static void saveBufferedImageToTypedFile(BufferedImage image,
			String fileType, String path) {
		try {
			File outputFile = new File(path);
			String name = outputFile.getName();
			assert isJpeg(name) || isPng(name) : "name ends with .jpg, .jpeg, or .png";

			ImageIO.write(image, fileType, outputFile);
			// dbg.sayln("... matrix " + path + " saved");
		} catch (IOException e) {
			throw new Error(
					"Error saving image in saveBufferedImageToTypedFile");
		}
	}

	/**
	 * Pauses the current thread for a given amount of time.
	 * 
	 * @param delayTimeInMilliseconds
	 *            number of milliseconds to pause the current thread
	 */
	public static void delay(int delayTimeInMilliseconds) {
		try {
			Thread.sleep(delayTimeInMilliseconds);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
}