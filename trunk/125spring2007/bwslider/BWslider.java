package bwslider;

import javax.swing.JFrame;
import javax.swing.JPanel;

import lotto.LottoPanel;

public class BWslider {

	public static void main(String[] args) {
		JFrame frame = new JFrame("Black and White Slider");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.setResizable(false);
		
		JPanel primary = new BWsliderPanel();
		
		frame.getContentPane().add(primary);
		frame.pack();
		frame.setVisible(true);
	}

}
