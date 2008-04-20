package intro;

import java.awt.Color;
import java.awt.Dimension;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

public class NestPanels {

	public static void main(String[] args) {
		JFrame frame = new JFrame("Nested Panels Demo");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		JPanel leftPanel = new JPanel();
		leftPanel.setBackground(Color.RED);
		leftPanel.setPreferredSize(new Dimension(150, 100));
		leftPanel.add(new JLabel("left"));

		JPanel rightPanel = new JPanel();
		rightPanel.setBackground(Color.YELLOW);
		rightPanel.setPreferredSize(new Dimension(150, 100));
		rightPanel.add(new JLabel("right"));

		JPanel primary = new JPanel();
		primary.setBackground(Color.BLUE);
		primary.add(leftPanel);
		primary.add(rightPanel);

		frame.getContentPane().add(primary);
		frame.pack();
		frame.setVisible(true);
	}

}
