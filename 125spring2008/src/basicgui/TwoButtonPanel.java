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

    public TwoButtonPanel() {
        // create widgets
        redButton = new JButton("Red");
        greenButton = new JButton("Green");

        redButton.setBackground(Color.RED);
        greenButton.setBackground(Color.GREEN);

        // add listeners
        ButtonListener listener = new ButtonListener();
        redButton.addActionListener(listener);
        greenButton.addActionListener(listener);

        // add widgets to this panel
        add(redButton);
        add(greenButton);

        // set the panel size
        setPreferredSize(new Dimension(200, 200));
    }

    private class ButtonListener implements ActionListener {
        public void actionPerformed(ActionEvent event) {
            if (event.getSource().equals(greenButton)) {
                setBackground(Color.GREEN);
            } else if (event.getSource().equals(redButton)) {
                setBackground(Color.RED);
            }
        }
    }


}
