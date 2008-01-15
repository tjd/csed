package basicgui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTextArea;

public class TextAreaWithButtonsPanel extends JPanel {

    private JButton pizzaButton;
    private JButton iceCreamButton;
    private JTextArea textBox;

    public TextAreaWithButtonsPanel() {
        // create widgets
        pizzaButton = new JButton("Pizza");
        iceCreamButton = new JButton("Ice Cream");      
        JPanel buttonPanel = new JPanel();
        buttonPanel.add(pizzaButton);
        buttonPanel.add(iceCreamButton);
        
        textBox = new JTextArea(10, 40);   // height, width
        
        // add listeners
        ButtonListener listener = new ButtonListener();
        pizzaButton.addActionListener(listener);
        iceCreamButton.addActionListener(listener);
        
        // add to panel
        this.setLayout(new BorderLayout());
        add(buttonPanel, BorderLayout.SOUTH);
        add(textBox, BorderLayout.CENTER);
    }

    private class ButtonListener implements ActionListener {
        public void actionPerformed(ActionEvent event) {
            if (event.getSource().equals(pizzaButton)) {
                textBox.append("Pizza!\n");
            } else if (event.getSource().equals(iceCreamButton)) {
                textBox.append("Ice cream!\n");
            }
        }
    }
}
