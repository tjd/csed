/*
 * Created on 13-May-2005
 */

package cschatbot;


/**
 * Handles the interfaces for JabberBotFramework, SwingBotFramework &
 * ConsoleBotFramework
 * 
 * @author Daryl Van Humbeck (raceimaztion)
 */
public abstract class BotFramework {
	public abstract void addMessageListener(MessageListener ml);

	public abstract void close();

	public abstract void sendMessageFinal(String msg, String to)
			throws XMPPException;

	/**
	 * Sends a text message to someone
	 * 
	 * @param message
	 *            The message to send
	 * @param to
	 *            The name to send it to
	 */
	public void sendMessage(String msg, String to) throws XMPPException {
		String[] lines = msg.split("\n");
		for (String element : lines) {
			sendMessageFinal(element, to);
		}
	}
}
