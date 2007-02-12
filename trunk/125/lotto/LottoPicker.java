package lotto;

import javax.swing.JFrame;
import javax.swing.JPanel;

/* 
 * In this lottery, 6 numbers (with no duplicates) are chosen in the range 
 * 1 to 49 inclusive.
 * 
 */

public class LottoPicker {

	public static void main(String[] args) {
		JFrame frame = new JFrame("Lotto Picker");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		JPanel primary = new LottoPanel();

		frame.getContentPane().add(primary);
		frame.pack();
		frame.setVisible(true);
	}

}
