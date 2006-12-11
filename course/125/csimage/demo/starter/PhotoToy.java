package csimage.demo.starter;

/*
 * Sample application showing how to use UberImage in a Swing GUI.
 */

import javax.swing.JFrame;

public class PhotoToy {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		JFrame frame = new JFrame("Photo Toy");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		frame.getContentPane().add(new PanelManager(frame));

		frame.pack();
		frame.setVisible(true);
	}

}
