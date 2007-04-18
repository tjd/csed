package buttontest;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

public class NamedPressPanel extends JPanel {
	private int count;

	private JButton button;

	private JLabel countLabel;

	public NamedPressPanel(String buttonText) {
		count = 0;
		button = new JButton(buttonText);
		button.addActionListener(new ButtonListener());

		countLabel = new JLabel();
		countLabel.setText("0");
		this.add(button);
		this.add(countLabel);
		this.setPreferredSize(new Dimension(120, 50));
	}

	private class ButtonListener implements ActionListener {
		public void actionPerformed(ActionEvent event) {
			count = count + 1;
			countLabel.setText("" + count);
		}
	}
}
