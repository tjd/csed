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
 *   
 * If this program is given as an assignment, then the following concepts need to be
 * discussed first:
 * 
 *   - Creating classes with main methods.
 *   - Class variables and methods.
 *   - The basic distinction between public and private.
 *   - The use of data types and declarations in Java.
 *   - Creation of objects using "new".
 *   - Basic use of ArrayList<String>.
 *   - Importing and using standard-library code.
 */

public class InspireMe2 implements ActionListener {

	// the following are private class variables: any method in the class
	// may access them, but code outside of the class may not access them
	private JFrame frame;

	private JLabel label;

	private JButton button;

	private ArrayList<String> message;

	private int curr;

	private void createAndShowGUI() {
		// add window decorations; try using "false" instead to see difference
		JFrame.setDefaultLookAndFeelDecorated(true);

		// create the frame
		frame = new JFrame("Inspire Me");

		// when the users closes the window, also end the program
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		// create a panel for the on-screen items
		JPanel panel = new JPanel();

		// tell the panel to arrange things using the BorderLayout style
		panel.setLayout(new BorderLayout());

		// tell the frame to arrange its components in the BorderLayout style
		panel.setLayout(new BorderLayout());

		// create the label with an inspirational message
		label = new JLabel("No matter where you go, there you are!");

		// create the button to press for a new message
		button = new JButton("Inspire me!");

		// tell the button to use this class to handle mouse clicks
		button.addActionListener(this);

		// put the label in the middle of the panel
		panel.add(label, BorderLayout.CENTER);

		// put the button under the label
		panel.add(button, BorderLayout.WEST);

		// add the panel to the frame
		frame.getContentPane().add(panel);

		// size the window to fit the layout
		frame.pack();

		// make the window visible on-screen
		frame.setVisible(true);
	}

	private void createMessages() {
		// create an empty ArrayList that holds only Strings
		message = new ArrayList<String>();

		// add as many messages as you want
		message.add("No matter where you go, there you are!");
		message.add("There's no time like the present.");
		message.add("There are no mistakes, only discoveries.");

		// curr tracks which message to display; start by showing the first
		// message
		curr = 0;

		// set the text of the label to be the current message
		label.setText(message.get(curr));
	}

	// The actionPerformed method is required to be here due to the "implements
	// ActionListener" in the class header. The idea is that an object of type
	// InspireMe1 is an action listener, i.e. it "listens" for actions such as
	// mouse-clicks or keypresses. The actionPerformed method is called whenever
	// the object it is listening too receives an event (in this case mouse
	// clicks), and it simply sits the label text to be the next message in the
	// message list.
	public void actionPerformed(ActionEvent event) {
		// increment the current message counter; wrap-around to 0 if necessary
		curr = (curr + 1) % message.size();

		// set the text of the label to be the current message
		label.setText(message.get(curr));

		// for debugging purposes, print the current message to the console
		// (should be removed when the program is finished)
		System.out.println(message.get(curr));
	}

	// The main method is special in Java: all programs must have a main method,
	// and Java automatically runs the code in the main method first.
	public static void main(String[] args) {
		// create an InspireMe1 object
		InspireMe2 im1 = new InspireMe2();

		// initialize the GUI and the messages
		im1.createAndShowGUI();
		im1.createMessages();
	}
}
