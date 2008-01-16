package fileviewer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import util.EasyInput;

public class FileViewerPanel extends JPanel {

	private JTextArea text;

	private JButton button;

	private JLabel title;

	public FileViewerPanel() {
		this.setLayout(new BorderLayout());

		// create the title
		title = new JLabel("File Viewer");
		title.setFont(new Font("Serif", Font.ITALIC, 16));

		JPanel titlePanel = new JPanel();
		titlePanel.add(title);
		titlePanel.setBackground(Color.PINK);
		this.add(titlePanel, BorderLayout.NORTH);

		// create the box to display the file
		text = new JTextArea(40, 40);
		text.setLineWrap(true);
		text.setEditable(false);
		// add scroll bars
		JScrollPane scrollPane = new JScrollPane(text);
		this.add(scrollPane, BorderLayout.CENTER);

		// add the "load file" button
		button = new JButton("Load File");
		button.setBackground(Color.WHITE);
		button.addActionListener(new ButtonListener());
		JPanel buttonPanel = new JPanel();
		buttonPanel.add(button);
		buttonPanel.setBackground(Color.BLUE);
		this.add(buttonPanel, BorderLayout.SOUTH);
	}

	private class ButtonListener implements ActionListener {
		public void actionPerformed(ActionEvent event) {
			String fname = EasyInput.chooseFile();
			String fileText = EasyInput.fileToString(fname);
			text.setText(fileText);
			title.setText(fname);
		}
	}

}
