package intro;

// based on listing 3.7 p.143 of LL5

import java.awt.Color;
import java.awt.Dimension;

import javax.swing.JFrame;
import javax.swing.JPanel;

public class GUIhelloWorld {
	
	public static void main(String[] args) {
		JFrame frame = new JFrame("Introduction");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		JPanel primary = new JPanel();
		primary.setBackground(Color.YELLOW);
		primary.setPreferredSize(new Dimension(250, 75));
		
		frame.getContentPane().add(primary);
		frame.setVisible(true);
	}
}
