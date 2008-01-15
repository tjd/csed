package basicgui;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JPanel;

public class TwoButtonPanel extends JPanel {

    private JButton redButton;
    private JButton greenButton;
    
    // thisPanel used to refer this TwoButtonPanel inside the listener classes
    private JPanel thisPanel = this;
    
    public TwoButtonPanel() {
        // create widgets
        redButton = new JButton("Red");
        greenButton = new JButton("Green");
        
        redButton.setBackground(Color.RED);
        greenButton.setBackground(Color.GREEN);
        
        // add listeners
        redButton.addActionListener(new RedButtonListener());
        greenButton.addActionListener(new GreenButtonListener());
        
        // add widgets to this panel
        this.add(redButton);
        this.add(greenButton);
        
        // set the panel size
        this.setPreferredSize(new Dimension(200, 200));
    }
    
    // create one listener class for each button
    private class RedButtonListener implements ActionListener {
        public void actionPerformed(ActionEvent event) {
            thisPanel.setBackground(Color.RED);
        }
    }
    
    private class GreenButtonListener implements ActionListener {
        public void actionPerformed(ActionEvent event) {
            thisPanel.setBackground(Color.GREEN);
        }
    }
    
}
