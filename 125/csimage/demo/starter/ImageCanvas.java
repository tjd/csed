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
		this.img = UberImage.blankImage(640, 400, Color.DARK_GRAY);
		this.setSize();
	}

	@Override
	public void paint(Graphics g) {
		g.drawImage(this.img, 20, 20, this);
	}

	public void setImage(UberImage img) {
		this.img = img;
		this.setSize();
	}

	public UberImage getImageCopy() {
		return UberImage.fromImage(this.img);
	}

	public void saveAsPNG(String fname) {
		this.img.saveAsPNG(fname);
	}

	public void setSize() {
		this.setSize(new Dimension(this.img.getWidth() + 40, this.img.getHeight() + 10));
		this.frame.pack();
		this.repaint();
	}

}
