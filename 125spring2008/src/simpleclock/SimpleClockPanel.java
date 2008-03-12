package simpleclock;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Date;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.Timer;

public class SimpleClockPanel extends JPanel {
	private JLabel label;

	public SimpleClockPanel() {
		// prepare the panel
		this.setPreferredSize(new Dimension(400, 50));
		this.setBackground(Color.WHITE);
		
		// create the label for the time
		label = new JLabel();
		label.setFont(new Font("Helvetica", Font.BOLD, 24));
		label.setForeground(Color.BLUE);
		this.add(label);
		
		// create the timer to update ever 1000 milliseconds = 1 second
		Timer timer = new Timer(1000, new MyActionListener());
		timer.start();		
	}

	private class MyActionListener implements ActionListener {
		public void actionPerformed(ActionEvent evt) {
			String now = "" + new Date();
			label.setText(now);
		}
	}
}
