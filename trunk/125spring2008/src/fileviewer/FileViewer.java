package fileviewer;

import javax.swing.JFrame;
import javax.swing.JPanel;

public class FileViewer {

	public static void main(String[] args) {
		JFrame frame = new JFrame("Simple File Viewer");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		JPanel primary = new FileViewerPanel();

		frame.getContentPane().add(primary);
		frame.pack();
		frame.setVisible(true);
	}

}
