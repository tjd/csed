package textfiles;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import util.EasyInput;

public class TextEditorPanel extends JPanel {

	private JButton loadButton;
	private JButton saveButton;
	private JTextArea text;

	public TextEditorPanel() {
		this.setLayout(new BorderLayout());

		// create the box to display the file
		text = new JTextArea(40, 40);
		text.setLineWrap(true);
		text.setEditable(true);
		// add scroll bars
		JScrollPane scrollPane = new JScrollPane(text);
		this.add(scrollPane, BorderLayout.CENTER);

		// add the buttons
		loadButton = new JButton("Load File");
		saveButton = new JButton("Save File");

		loadButton.addActionListener(new ButtonListener());
		saveButton.addActionListener(new ButtonListener());

		JPanel buttonPanel = new JPanel();
		buttonPanel.add(loadButton);
		buttonPanel.add(saveButton);
		this.add(buttonPanel, BorderLayout.SOUTH);
	}

	private class ButtonListener implements ActionListener {
		public void actionPerformed(ActionEvent event) {
			if (event.getSource().equals(loadButton)) {
				String fname = EasyInput.chooseFile();
				String fileText = EasyInput.fileToString(fname);
				text.setText(fileText);
			} else if (event.getSource().equals(saveButton)) {
				try {
					FileWriter fw = new FileWriter("sample.txt");
					BufferedWriter bw = new BufferedWriter(fw);
					PrintWriter outFile = new PrintWriter(bw);
					String[] lines = text.getText().split("\n");
					for (String line : lines) {
						outFile.write(line + "\n");
					}
					System.out.printf("sample.txt written to %s", EasyInput.getcwd());
				} catch (IOException e) {
					e.printStackTrace();
				}

			}

		}
	}
}
