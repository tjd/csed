package buttontest;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

public class PressPanel extends JPanel {

	private int count;

	private JButton button;

	private JLabel countLabel;

	public PressPanel() {
		count = 0;
		button = new JButton("Click me!");
		button.addActionListener(new ButtonListener());

		countLabel = new JLabel();
		countLabel.setText("0");
		this.add(button);
		this.add(countLabel);
		this.setPreferredSize(new Dimension(300, 40));
	}

	private class ButtonListener implements ActionListener {
		public void actionPerformed(ActionEvent event) {
			count = count + 1;
			countLabel.setText("" + count);
		}
	}
}
