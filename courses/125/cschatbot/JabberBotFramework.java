/*
 * Created on 6-May-2005
 */

package cschatbot;

import java.util.Vector;

import com.sun.corba.se.impl.protocol.giopmsgheaders.Message;

/**
 * Bare minimum Jabber chatbot framework Allows user to add a MessageListener to
 * listen for message Also allows user to send messages
 * 
 * @author Daryl Van Humbeck (raceimaztion)
 */
public class JabberBotFramework extends BotFramework implements PacketListener {
	private String hostName;

	private XMPPConnection mainConnection;

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
	public JabberBotFramework(String host, String username, String password)
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
	public JabberBotFramework(String host, String username, String password,
			int port) throws XMPPException {
		messageListeners = new Vector();

		hostName = host;
		if (port < 0) {
			mainConnection = new XMPPConnection(host);
		} else {
			mainConnection = new XMPPConnection(host, port);
		}
		mainConnection.login(username, password);

		mainConnection.addPacketListener(this, new PacketTypeFilter(
				Message.class));
	}

	/**
	 * Breaks the connection to the server
	 */
	@Override
	public void close() {
		mainConnection.close();
	}

	public void processPacket(Packet p) {
		if ((p != null) && (p instanceof Message)) {
			Message msg = (Message) p;
			for (int i = 0; i < messageListeners.size(); i++) {
				MessageListener ml = (MessageListener) messageListeners.get(i);
				ml.messageRecieved(msg);
			}
		}
	}

	/**
	 * Adds a MessageListener to the list of listeners
	 * 
	 * @param ml
	 *            The listener to add
	 */
	@Override
	public void addMessageListener(MessageListener ml) {
		messageListeners.add(ml);
	}

	/**
	 * Sends a text message to someone
	 * 
	 * @param message
	 *            The message to send
	 * @param to
	 *            The name to send it to
	 */
	@Override
	public void sendMessageFinal(String message, String to)
			throws XMPPException {
		Chat c = mainConnection.createChat(to);
		c.sendMessage(message);
	}
}
