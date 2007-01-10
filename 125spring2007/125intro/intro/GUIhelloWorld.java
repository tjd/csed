package intro;

import java.awt.Color;
import java.awt.Dimension;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JLabel;

public class GUIhelloWorld {
	
	public static void main(String[] args) {
		JFrame frame = new JFrame("Introduction");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		JPanel primary = new JPanel();
		primary.setBackground(Color.YELLOW);
		primary.setPreferredSize(new Dimension(210, 75));
		
		JLabel line1 = new JLabel("No matter where you go");
		primary.add(line1);
		JLabel line2 = new JLabel("there you are");
		primary.add(line2);
		
		frame.getContentPane().add(primary);
		frame.pack();
		frame.setVisible(true);
	}
}
