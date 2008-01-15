package basicgui;

import java.awt.Dimension;

import javax.swing.JLabel;
import javax.swing.JPanel;

public class OneLabelPanel extends JPanel {

    public OneLabelPanel(String msg) {
        JLabel label = new JLabel(msg);
        
        add(label);
        setPreferredSize(new Dimension(100, 40));
    }
    
}
