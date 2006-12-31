/*
 * Created on 14-May-2005
 */
package cschatbot;

import java.io.*;
import java.util.*;

import org.jivesoftware.smack.*;
import org.jivesoftware.smack.packet.*;

/**
 * @author Daryl Van Humbeck (raceimaztion)
 */
public class ConsoleBotFramework extends BotFramework implements Runnable {
	private boolean done = false;

	private String botName;

	private Vector messageListeners;

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
	public ConsoleBotFramework(String host, String username, String password)
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
	public ConsoleBotFramework(String host, String username, String password,
			int port) throws XMPPException {
		botName = username;
		messageListeners = new Vector();

		(new Thread(this)).start();
	}

	public void run() {
		try {
			BufferedReader in = new BufferedReader(new InputStreamReader(
					System.in));

			String input;
			while (!done) {
				while (!in.ready()) {
					if (done)
						return;
					try {
						Thread.sleep(10);
					} catch (InterruptedException er) {
					}
				}

				input = in.readLine();

				MessageListener ml;
				Message msg = new Message();
				msg.setFrom("User");
				msg.setBody(input);

				for (int i = 0; i < messageListeners.size(); i++) {
					ml = (MessageListener) messageListeners.get(i);
					ml.messageRecieved(msg);
				}
			}
		} catch (IOException er) {

		}
	}

	public void addMessageListener(MessageListener ml) {
		messageListeners.add(ml);
	}

	public void close() {
		done = true;
	}

	public void sendMessageFinal(String msg, String to) throws XMPPException {
		System.out.println(botName + ":\t" + msg);
	}
}
