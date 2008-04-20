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
		this.setLayout(new BorderLayout());
		ActionListener buttonListener = new ButtonListener();

		// create and configure the load/save buttons
		JPanel loadSavePanel = new JPanel();
		this.add(loadSavePanel, BorderLayout.SOUTH);
		this.loadButton = new JButton("Load");
		this.loadButton.addActionListener(buttonListener);
		loadSavePanel.add(this.loadButton);
		this.saveButton = new JButton("Save");
		this.saveButton.addActionListener(buttonListener);
		loadSavePanel.add(this.saveButton);

		// create and configure the effect buttons
		JPanel effectPanel = new JPanel();
		this.add(effectPanel, BorderLayout.NORTH);
		this.bwButton = new JButton("black & white");
		this.bwButton.addActionListener(buttonListener);
		effectPanel.add(this.bwButton);

		// create and congfigure the image canvas
		this.canvas = new ImageCanvas(frame);
		this.add(this.canvas, BorderLayout.CENTER);
	}

	public class ButtonListener implements ActionListener {

		public void actionPerformed(ActionEvent event) {
			if (event.getSource() == PanelManager.this.loadButton) {
				PanelManager.this.fname = EasyInput.chooseFile();
				PanelManager.this.canvas.setImage(UberImage
						.fromFile(PanelManager.this.fname));
			} else if (event.getSource() == PanelManager.this.saveButton) {
				PanelManager.this.canvas.saveAsPNG(this.getName() + "_mod.png");
			} else if (event.getSource() == PanelManager.this.bwButton) {
				UberImage pic = PanelManager.this.canvas.getImageCopy();
				Effects.makeBlackAndWhite(pic);
				PanelManager.this.canvas.setImage(pic);
			}
		}

		// helper function that returns the name, without the
		// extension, of the current file being processed
		private String getName() {
			int dotLoc = PanelManager.this.fname.lastIndexOf(".");
			return PanelManager.this.fname.substring(0, dotLoc);
		}

	}

}
