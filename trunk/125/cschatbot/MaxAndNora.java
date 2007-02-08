/*
 * Created on 13-May-2005
 */

package cschatbot;

import javax.swing.JOptionPane;

import org.jivesoftware.smack.XMPPException;

import com.sun.corba.se.impl.protocol.giopmsgheaders.Message;

/**
 * @author Daryl Van Humbeck (raceimaztion) Plays Max & Nora with anyone who
 *         sends messages to it. A quick description of Max & Nora is: one
 *         person (the user) asks questions of the other (the chatbot) and gets
 *         either a yes or no answer back, depending on the first letter of the
 *         last word in the player's question. If it's m or earlier, the bot
 *         responds Yes, otherwise No.
 */
public class MaxAndNora implements MessageListener {
	private BotFramework jbf;

	/**
	 * Creates an instance of MaxAndNora
	 * 
	 * @param j
	 *            A pre-initialized copy of BotFramework (ie, logged-in)
	 */
	public MaxAndNora(BotFramework j) {
		// Just store the object
		this.jbf = j;
	}

	/*
	 * This is called whenever someone sends us a message
	 */
	public void messageRecieved(Message msg) {
		// Is this a question:
		String body = msg.getBody().toLowerCase();
		if (body.endsWith("?")) {
			// Yes, get the last word of the message content
			String[] words = body.split(" ");
			String lastWord = words[words.length - 1];
			while (!lastWord.matches("[a-z].*")) {
				lastWord = lastWord.substring(1);
			}

			// Send a message back to the sender
			try {
				if (lastWord.charAt(0) < 'n') {
					jbf.sendMessage("Yes", msg.getFrom());
				} else {
					jbf.sendMessage("No", msg.getFrom());
				}
			} catch (XMPPException er) {
				// Do nothing
			}
		} else {
			// No, inform the user what this is:
			try {
				jbf
						.sendMessage(
								"This chatbot plays Max and Nora.\nPlease ask me yes or no questions.",
								msg.getFrom());
			} catch (XMPPException er) {
				// Do nothing
			}
		}
	}

	/**
	 * Starts the Max&Nora chatbot
	 */
	public static void main(String[] args) {
		try {
			// BotFramework jbf = new ConsoleBotFramework("jabber.org",
			// "csjavaChatBot", "dumbPassword");
			BotFramework jbf = new JabberBotFramework("jabber.org",
					"csjavaChatBot", "dumbPassword");
			System.out.println("Instance of JabberBotFramework created");
			jbf.addMessageListener(new MaxAndNora(jbf));
			System.out.println("Ready and waiting for messages.");

			JOptionPane.showMessageDialog(null,
					"Click OK to shutdown the bot.", "Jabber chatBot test",
					JOptionPane.INFORMATION_MESSAGE);

			jbf.close();
			System.out.println("Connection to server closed");
			System.exit(0);
		} catch (XMPPException er) {
			System.out
					.println("Error setting up and using Jabber chatbot framework!");
			System.out.println(er);
		}
	}
}
