package lotto;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collections;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

public class LottoPanel extends JPanel {

	private ArrayList<Integer> numbers;

	private ArrayList<JLabel> label;

	private JButton pickButton;

	public LottoPanel() {
		// create the list of all 49 numbers
		numbers = new ArrayList<Integer>();
		for (int i = 1; i <= 49; ++i) {
			numbers.add(i);
		}

		// create the 6 labels
		label = new ArrayList<JLabel>();
		for (int i = 0; i < 6; ++i) {
			label.add(new JLabel("?"));
		}

		// create the button
		pickButton = new JButton("Pick");
		pickButton.addActionListener(new ButtonListener());

		// add the GUI components
		for (int i = 0; i < 6; ++i) {
			this.add(label.get(i));
		}
		this.add(pickButton);
		this.setPreferredSize(new Dimension(165, 60));

		selectNumbers();
	}

	private void selectNumbers() {
		Collections.shuffle(numbers);
		for (int i = 0; i < 6; ++i) {
			label.get(i).setText("" + numbers.get(i));
		}
	}

	private class ButtonListener implements ActionListener {
		public void actionPerformed(ActionEvent event) {
			selectNumbers();
		}
	}

}
