package basicgui;

import javax.swing.JFrame;
import javax.swing.JPanel;

public class FlipCoin {

    public static void main(String[] args) {
        JFrame frame = new JFrame("Coin Flip");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        JPanel primary = new FlipCoinPanel();
        
        // add the primary panel to the frame
        frame.getContentPane().add(primary);
        frame.pack();
        frame.setVisible(true);
    }

}
