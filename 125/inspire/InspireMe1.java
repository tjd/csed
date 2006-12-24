package inspire; // packages help organize programs

// import the necessary classes from the Java library
import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

/*
 * This file is based in part on the Java Tutorial Swing trail: 
 *   
 *    http://java.sun.com/docs/books/tutorial/uiswing/learn/
 *    
 * Objectives for this case study:   
 * 
 *   - Become familiar with Eclipse.
 *   - Learn basic Java syntax and conventions:
 *       - Code blocks.
 *       - Basic statements.
 *       - Dot-notation for accessing object attributes and methods.
 *       - Source code comments.
 *   - Import and use library classes.
 *   - Create a simple class.
 *   - Create simple functions.
 *   - Create a main method.
 *   - Create a simple GUI.
 *   - Create a package.
 */

public class InspireMe1 implements ActionListener {

	// the following are private class variables: any method in the class
	// may access them, but code outside of the class may not access them
	private JFrame frame;

	private JPanel panel;

	private JLabel label;

	private JButton button;

	private ArrayList<String> message;

	private int curr;

	/**
	 * Create the GUI and show it.
	 */
	private void createAndShowGUI() {
		// add window decorations; try using "false" instead to see difference
		JFrame.setDefaultLookAndFeelDecorated(true);

		// create the frame
		frame = new JFrame("Inspire Me");

		// when the users closes the window, also end the program
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		// create the label with an inspirational message
		label = new JLabel("No matter where you go, there you are!");

		// create the button to press for a new message
		button = new JButton("Inspire me!");

		button.addActionListener(this);

		// create a panel with to hold the the label and button
		panel = new JPanel();

		// tell the panel to arrange its components in the BorderLayout style
		panel.setLayout(new BorderLayout());

		// put the label in the middle of the panel
		panel.add(label, BorderLayout.CENTER);

		// put the button under the label
		panel.add(button, BorderLayout.SOUTH);

		// add the panel to the frame's content pane
		frame.getContentPane().add(panel);

		// size the window to fit the layout
		frame.pack();

		// make the window visible on-screen
		frame.setVisible(true);
	}

	public void createMessages() {
		// create an empty ArrayList that holds only Strings
		message = new ArrayList<String>();
		
		// add the messages: add as many as you want
		message.add("No matter where you go, there you are!");
		message.add("There's no time like the present.");
		message.add("There are no mistakes, only discoveries.");

		// curr tracks which message to display; start by showing the first
		// message
		curr = 0;

		// set the text of the label to be the current message
		label.setText(message.get(curr));
	}

	public void actionPerformed(ActionEvent event) {
		// increment the current message counter; wrap-around to 0 if necessary
		curr = (curr + 1) % message.size();

		// set the text of the label to be the current message
		label.setText(message.get(curr));

		// for debugging purposes, print the current message to the console
		// (should be removed when the program is finished)
		System.out.println(message.get(curr));
	}

	public static void main(String[] args) {
		InspireMe1 im1 = new InspireMe1();
		im1.createAndShowGUI();
		im1.createMessages();
	}
}
