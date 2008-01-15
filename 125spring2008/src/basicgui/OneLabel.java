package basicgui;

import javax.swing.JFrame;
import javax.swing.JPanel;

public class OneLabel {

    public static void main(String[] args) {
        JFrame frame = new JFrame("One Label");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        JPanel primary = new OneLabelPanel("Hello, world!");

        // add the primary panel to the frame
        frame.getContentPane().add(primary);
        frame.pack();
        frame.setVisible(true);
    }

}
