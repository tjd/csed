package basicgui;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Random;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

public class FlipCoinPanel extends JPanel {
	private JButton flipButton;

	private JLabel flipResult;

	private Random rnd;

	public FlipCoinPanel() {
		// create random number generator
		rnd = new Random();

		// create widgets
		flipButton = new JButton("Flip");
		flipResult = new JLabel();
		flipCoin(); // give label an initial value
		
		// give the label text a big easy to read font
		flipResult.setFont(new Font("Helvetica", Font.BOLD, 48));

		// add listeners
		flipButton.addActionListener(new ButtonListener());

		// add to panel
		add(flipButton);
		add(flipResult);

		// set size
		setPreferredSize(new Dimension(220, 80));
	}

	private void flipCoin() {
		if (rnd.nextBoolean()) {
			flipResult.setText("heads");
		} else {
			flipResult.setText("tails");
		}
	}

	private class ButtonListener implements ActionListener {
		public void actionPerformed(ActionEvent event) {
			flipCoin();
		}
	}
}
