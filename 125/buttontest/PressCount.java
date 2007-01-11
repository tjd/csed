package buttontest;

import javax.swing.JFrame;
import javax.swing.JPanel;

public class PressCount {

	public static void main(String[] args) {
		JFrame frame = new JFrame("Button Press Counter");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		JPanel primary = new PressPanel();
		
		// add the primary panel to the frame
		frame.getContentPane().add(primary);
		frame.pack();
		frame.setVisible(true);
	}
}

