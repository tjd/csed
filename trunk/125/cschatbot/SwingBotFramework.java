/*
 * Created on 13-May-2005
 */

package cschatbot;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

import com.sun.corba.se.impl.protocol.giopmsgheaders.Message;

/**
 * Allows you to test a chatbot without using extra software
 * 
 * @author Daryl Van Humbeck (raceimaztion)
 */
public class SwingBotFramework extends BotFramework implements ActionListener {
	private Vector messageListeners;

	private JFrame window;

	private JTextField input;

	private JEditorPane textPane;

	private String botName;

	/**
	 * Logs into a Jabber server with the username and password provided
	 * 
	 * @param host
	 *            The servername to connect to
	 * @param username
	 *            The username to use
	 * @param password
	 *            The password to use
	 * @throws XMPPException
	 *             If any problems arise, this is thrown
	 */
	public SwingBotFramework(String host, String username, String password)
			throws XMPPException {
		this(host, username, password, -1);
	}

	/**
	 * Logs into a Jabber server and port number with the username and password
	 * provided
	 * 
	 * @param host
	 *            The servername to connect to
	 * @param username
	 *            The username to use
	 * @param password
	 *            The password to use
	 * @param port
	 *            The port number to use
	 * @throws XMPPException
	 *             If any problems arise, this is thrown
	 */
	public SwingBotFramework(String host, String username, String password,
			int port) throws XMPPException {
		// Don't need any of the parameters really, just for compatibilities'
		// sake
		messageListeners = new Vector();

		botName = username;

		window = new JFrame("SwingBotFramework [" + username + "]");
		window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		input = new JTextField();
		input.addActionListener(this);
		textPane = new JEditorPane("text/plain", "");
		textPane.setEditable(false);
		JButton b = new JButton("Send");
		b.addActionListener(this);
		JScrollPane pane = new JScrollPane(textPane);
		pane.setPreferredSize(new Dimension(400, 300));

		JPanel jp = new JPanel(new BorderLayout());
		jp.add(input, BorderLayout.CENTER);
		jp.add(b, BorderLayout.EAST);

		Container c = window.getContentPane();
		c.add(jp, BorderLayout.SOUTH);
		c.add(pane, BorderLayout.CENTER);

		window.pack();
		window.setVisible(true);
	}

	public void actionPerformed(ActionEvent e) {
		String s = input.getText();
		input.setText("");

		String text = textPane.getText();
		if (text.length() > 1) {
			text += "\nUser:\t" + s;
		} else {
			text = "User:\t" + s;
		}
		textPane.setText(text);

		Message msg = new Message();
		msg.setBody(s);
		msg.setFrom("User");

		if (messageListeners.size() > 0) {
			MessageListener ml;
			for (int i = 0; i < messageListeners.size(); i++) {
				ml = (MessageListener) messageListeners.get(i);
				ml.messageRecieved(msg);
			}
		}
	}

	@Override
	public void addMessageListener(MessageListener ml) {
		messageListeners.add(ml);
	}

	@Override
	public void close() {
		window.setVisible(false);
		window.dispose();
	}

	@Override
	public void sendMessageFinal(String msg, String to) throws XMPPException {
		// Ignores "to" parameter
		String text = textPane.getText();
		text += "\n" + botName + ":\t" + msg;
		textPane.setText(text);
	}
}
