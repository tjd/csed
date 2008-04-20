/*
 * Created on 6-May-2005
 */

package cschatbot;

// import org.jivesoftware.smack.*;
import com.sun.corba.se.impl.protocol.giopmsgheaders.Message;

/**
 * @author Daryl Van Humbeck (raceimaztion)
 * 
 * For classes wanting to know when we've recieved a message
 */
public interface MessageListener {
	public void messageRecieved(Message msg);
}
