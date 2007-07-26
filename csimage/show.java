package csimage;

/**
 * 
 * Utility methods for displaying BufferedImage objects.
 * 
 */


import java.awt.Graphics;
import java.awt.Insets;
import java.awt.image.BufferedImage;

import javax.swing.JFrame;
import javax.swing.JPanel;

public class show {

	/**
	 * Display the given image in a frame.
	 * 
	 * @param fname
	 *            name of the file with the image
	 */
	public static void inFrame(String fname) {
		inFrame(UberImage.fromFile(fname));
	}

	/**
	 * 
	 * Displays the given BufferedImage in a frame.
	 * 
	 * @param img
	 *            the BufferedImage to display
	 */
	public static void inFrame(final BufferedImage img) {
		javax.swing.SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				JFrame f = makeFrame(img);
				f.setVisible(true);
			}
		});
	}

	// //////////////////////////////////////////////////////////////

	public static JFrame makeFrame(final BufferedImage img) {
		assert img != null;
		JPanel panel = new JPanel() {
			public void paintComponent(Graphics g) {
				super.paintComponents(g);
				g.drawImage(img, 0, 0, this);
			}
		};
		panel.setSize(img.getWidth(), img.getHeight());

		JFrame f = new JFrame("Image Frame");
		f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		f.setContentPane(panel);

		f.setSize(panel.getWidth() + insets.left + insets.right, panel
				.getHeight()
				+ insets.top + insets.bottom);

		// center the frame
		f.setLocationRelativeTo(null);

		f.setResizable(false);

		return f;
	}

	public final static Insets insets = getJFrameInsets();

	// Gets the insets of a JFrame without displaying a window.
	public static Insets getJFrameInsets() {
		JFrame f = new JFrame();
		f.pack();
		Insets insets = f.getInsets();
		f.dispose();
		return insets;
	}

	public static void main(String[] args) {
		show.inFrame(UberImage.fromFile("csimage/pictures/Raphael.jpg"));
	}
}
