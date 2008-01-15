package basicgui;

import javax.swing.JFrame;
import javax.swing.JPanel;

public class VoteCount {

	public static void main(String[] args) {
		JFrame frame = new JFrame("Vote Counter");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		JPanel primary = new JPanel();
		JPanel yes = new PressPanel("yes");
		JPanel no = new PressPanel("no");
		JPanel spoiled = new PressPanel("spoiled");
		primary.add(yes);
		primary.add(no);
		primary.add(spoiled);

		// add the primary panel to the frame
		frame.getContentPane().add(primary);
		frame.pack();
		frame.setVisible(true);
	}

}
