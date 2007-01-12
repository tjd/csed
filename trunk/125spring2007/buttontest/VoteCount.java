package buttontest;

import javax.swing.JFrame;
import javax.swing.JPanel;

public class VoteCount {


	public static void main(String[] args) {
		JFrame frame = new JFrame("Vote Counter");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		JPanel primary = new JPanel();
		JPanel yes = new NamedPressPanel("yes");
		JPanel no = new NamedPressPanel("no");
		JPanel spoiled = new NamedPressPanel("spoiled");
		primary.add(yes);
		primary.add(no);
		primary.add(spoiled);
		
		// add the primary panel to the frame
		frame.getContentPane().add(primary);
		frame.pack();
		frame.setVisible(true);

	}

}
