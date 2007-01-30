package bwslider;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import util.EasyInput;

import csimage.UberImage;

public class BWsliderPanel extends JPanel {

	private UberImage img;
	private UberImage copy;

	private final int SLIDER_MIN = 0;

	private final int SLIDER_MAX = 100;

	public double brightness(Color c) {
		return (c.getRed() + c.getGreen() + c.getBlue()) / (3.0 * 255);
	}

	public Color convertToBW(Color c, double threshold) {
		if (brightness(c) < threshold) {
			return Color.BLACK;
		} else {
			return Color.white;
		}
	}

	public void convertToBW(UberImage orig, UberImage dest, double threshold) {
		assert 0 <= threshold && threshold <= 1;
		assert orig.getWidth() == dest.getWidth() && orig.getHeight() == dest.getHeight();
		for (int i = 0; i < orig.getWidth(); ++i) {
			for (int j = 0; j < orig.getHeight(); ++j) {
				dest.setColor(i, j, convertToBW(orig.getColor(i, j), threshold));
			}
		}
	}

	public BWsliderPanel() {
		JSlider slider = new JSlider(JSlider.HORIZONTAL, SLIDER_MIN,
				SLIDER_MAX, 50);
		slider.addChangeListener(new SliderListener());
		// Turn on labels at major tick marks.
		slider.setMajorTickSpacing(50);
		slider.setMinorTickSpacing(5);
		slider.setPaintTicks(true);
		slider.setPaintLabels(true);

		JPanel panel = new ImagePanel();
		img = UberImage.fromFile(EasyInput.chooseFile());
		copy = new UberImage(img);
		panel.setPreferredSize(new Dimension(img.getWidth(), img.getHeight()));

		this.setLayout(new BorderLayout());
		this.add(BorderLayout.CENTER, panel);
		this.add(BorderLayout.SOUTH, slider);
	}

	private class ImagePanel extends JPanel {
		public void paint(Graphics g) {
			g.drawImage(img, 0, 0, null);
		}
	}

	private class SliderListener implements ChangeListener {
		public void stateChanged(ChangeEvent e) {
			JSlider source = (JSlider) e.getSource();
			if (!source.getValueIsAdjusting()) {
				int thresh = source.getValue();
				convertToBW(copy, img, thresh / (double) SLIDER_MAX);
				repaint();
				System.out.println("" + thresh + "%");
			}
		}
	}

}
