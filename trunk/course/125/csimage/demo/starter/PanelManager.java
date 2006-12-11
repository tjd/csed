package csimage.demo.starter;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;

import csimage.UberImage;
import csimage.util.EasyInput;

public class PanelManager extends JPanel {
	// buttons
	private JButton loadButton, saveButton;

	private JButton bwButton;

	// the canvas the picture appears on
	private ImageCanvas canvas;

	// name of the most recently loaded image file
	private String fname;

	// the frame this panel is in
	private JFrame frame;

	public PanelManager(JFrame frame) {
		// initialize this panel
		this.frame = frame;
		setLayout(new BorderLayout());
		ActionListener buttonListener = new ButtonListener();

		// create and configure the load/save buttons
		JPanel loadSavePanel = new JPanel();
		add(loadSavePanel, BorderLayout.SOUTH);
		loadButton = new JButton("Load");
		loadButton.addActionListener(buttonListener);
		loadSavePanel.add(loadButton);
		saveButton = new JButton("Save");
		saveButton.addActionListener(buttonListener);
		loadSavePanel.add(saveButton);

		// create and configure the effect buttons
		JPanel effectPanel = new JPanel();
		add(effectPanel, BorderLayout.NORTH);
		bwButton = new JButton("black & white");
		bwButton.addActionListener(buttonListener);
		effectPanel.add(bwButton);

		// create and congfigure the image canvas
		canvas = new ImageCanvas(frame);
		add(canvas, BorderLayout.CENTER);
	}

	public class ButtonListener implements ActionListener {

		public void actionPerformed(ActionEvent event) {
			if (event.getSource() == loadButton) {
				fname = EasyInput.chooseFile();
				canvas.setImage(UberImage.fromFile(fname));
			} else if (event.getSource() == saveButton) {
				canvas.saveAsPNG(getName() + "_mod.png");
			} else if (event.getSource() == bwButton) {
				UberImage pic = canvas.getImageCopy();
				Effects.makeBlackAndWhite(pic);
				canvas.setImage(pic);
			}
		}

		// helper function that returns the name, without the
		// extension, of the current file being processed
		private String getName() {
			int dotLoc = fname.lastIndexOf(".");
			return fname.substring(0, dotLoc);
		}

	}

}
