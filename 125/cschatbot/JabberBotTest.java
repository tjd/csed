/*
 * Created on 12-May-2005
 */

package cschatbot;

import javax.swing.JOptionPane;

import org.jivesoftware.smack.XMPPException;

import com.sun.corba.se.impl.protocol.giopmsgheaders.Message;

/**
 * @author Daryl Van Humbeck (raceimaztion)
 */
public class JabberBotTest implements MessageListener {
	private JabberBotFramework bot;

	public JabberBotTest(JabberBotFramework bot) {
		this.bot = bot;
	}

	public void messageRecieved(Message msg) {
		System.out.println("Message recieved:\n" + msg.getBody());
		try {
			bot.sendMessage("Booyah!!", msg.getFrom());
		} catch (XMPPException er) {
			// Do nothing
		}
	}

	public static void main(String[] args) {
		try {
			JabberBotFramework jbf = new JabberBotFramework("jabber.org",
					"csjavaChatBot", "dumbPassword");
			System.out.println("Instance of JabberBotFramework created");
			jbf.addMessageListener(new JabberBotTest(jbf));
			System.out.println("Ready and waiting for messages.");

			JOptionPane.showMessageDialog(null,
					"Click OK to shutdown the bot.", "Jabber chatBot test",
					JOptionPane.INFORMATION_MESSAGE);

			jbf.close();
			System.out.println("Connection to server closed");
			System.exit(0);
		} catch (Throwable er) {
			System.out
					.println("Error setting up and using Jabber chatbot framework!");
			System.out.println(er);
		}
	}
}
