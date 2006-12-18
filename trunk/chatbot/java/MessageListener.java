

//import org.jivesoftware.smack.*;
import org.jivesoftware.smack.packet.*;

/**
 * @author Daryl Van Humbeck (raceimaztion)
 * 
 * For classes wanting to know when we've recieved a message
 */
public interface MessageListener
{
    public void messageRecieved(Message msg);
}
