package textfiles;

import javax.swing.JFrame;
import javax.swing.JPanel;

import fileviewer.FileViewerPanel;

public class TextEdit {

	public static void main(String[] args) {
		JFrame frame = new JFrame("Simple Text Editor");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		JPanel primary = new TextEditorPanel();

		frame.getContentPane().add(primary);
		frame.pack();
		frame.setVisible(true);
	}

}
