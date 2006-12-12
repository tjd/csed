package csimage.demo.starter;

import java.awt.Canvas;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;

import javax.swing.JFrame;

import csimage.UberImage;

public class ImageCanvas extends Canvas {

	private UberImage img;

	private JFrame frame;

	public ImageCanvas(JFrame frame) {
		this.frame = frame;
		img = UberImage.blankImage(640, 400, Color.DARK_GRAY);
		setSize();
	}

	public void paint(Graphics g) {
		g.drawImage(img, 20, 20, this);
	}

	public void setImage(UberImage img) {
		this.img = img;
		setSize();
	}

	public UberImage getImageCopy() {
		return UberImage.fromImage(img);
	}

	public void saveAsPNG(String fname) {
		img.saveAsPNG(fname);
	}

	public void setSize() {
		setSize(new Dimension(img.getWidth() + 40, img.getHeight() + 10));
		frame.pack();
		repaint();
	}

}
