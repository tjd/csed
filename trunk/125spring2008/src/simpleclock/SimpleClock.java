package simpleclock;

import javax.swing.JFrame;
import javax.swing.JPanel;

public class SimpleClock {
	public static void main(String[] args) {
		JFrame frame = new JFrame("Simple Clock");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		JPanel primary = new SimpleClockPanel();

		// add the primary panel to the frame
		frame.getContentPane().add(primary);
		frame.pack();
		frame.setVisible(true);
	}

}
