/*
 * Created on 2-Jul-2005
 */

package misc;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import javax.swing.JButton;
import javax.swing.JFrame;

/**
 * @author Daryl Van Humbeck (Raceimaztion)
 * This class implements one way to select a method to call
 * for testing purposes. 
 */
public class MethodMenu extends JFrame implements ActionListener
{
    public boolean done = false;
    
    private Object owner;
    private Class ownerClass;
    private String[] methodNames, shownNames;
    private JButton[] methodButtons;
    
    private MethodMenu(Object owner, String[] methods, String[] names)
    {
        super("Pick one:");
        setDefaultCloseOperation(EXIT_ON_CLOSE);
        
        // Save information
        this.owner = owner;
        ownerClass = owner.getClass();
        methodNames = methods;
        shownNames = names;
        
        finishSetup();
    }
    
    private MethodMenu(Class owner, String[] methods, String[] names)
    {
        super("Pick one:");
        setDefaultCloseOperation(EXIT_ON_CLOSE);
        
        // Save information
        this.owner = null;
        ownerClass = owner;
        methodNames = methods;
        shownNames = names;
        
        finishSetup();
    }
    
    private void finishSetup()
    {
        // Create the buttons
        methodButtons = new JButton[shownNames.length];
        JButton cur;
        for (int i=0; i < shownNames.length; i++)
        {
            cur = new JButton(shownNames[i]);
            cur.addActionListener(this);
            methodButtons[i] = cur;
        }
        
        // Add the buttons to the window
        Container c = getContentPane();
        c.setLayout(new GridLayout(0, 1));
        for (JButton element : methodButtons) {
            c.add(element);
        }
        
        pack();
        
        // Center the window
        Toolkit tk = this.getToolkit();
        Dimension screenSize = tk.getScreenSize();
        setLocation((screenSize.width - getWidth())/2, (screenSize.height - getHeight())/2);
        
        setVisible(true);
    }
    
    // looks for an object in the list of buttons
    private int getIndex(Object o)
    {
        for (int i=0; i < methodButtons.length; i++) {
			if (o.equals(methodButtons[i])) {
				return i;
			}
		}
        
        return -1;
    }
    
    public void actionPerformed(ActionEvent e)
    {
        // look for the button pressed
        int index = getIndex(e.getSource());
        if (index < 0) {
			return;
		}
        
        // Now activate the method that was asked for
        try
        {
            System.out.print("Invoking method " + methodNames[index] + "...");
            
	        Method method = ownerClass.getMethod(methodNames[index], new Class[0]);
	        method.invoke(null, null);
	        
	        done = true;
	        setVisible(false);
	        dispose();
	        System.out.println("Done");
        }
        catch (NoSuchMethodException er) { System.out.println("No such method"); }
        catch (IllegalAccessException er) { System.out.println("Can't access method"); }
        catch (InvocationTargetException er) { System.out.println("Unknown error"); }
    }
    
    /**
     * Shows a list of methods to call as buttons
     * @param owner			The class to call the methods of
     * @param methodNames	The list of method names
     * @param shownNames	How to label each method
     */
    public static void showList(Object owner, String[] methodNames, String[] shownNames)
    {
        if (methodNames.length != shownNames.length) {
			throw new Error("The lists given to Test.showList() MUST be of the same length!");
		}
        
        MethodMenu t = new MethodMenu(owner, methodNames, shownNames);
        // Wait for the dialog to be done:
        while (!t.done)
        {
            try { Thread.sleep(500); }
            catch (InterruptedException er) { }
        }
    }

    /**
     * Shows a list of methods to call as buttons
     * @param owner			The class to call the methods of
     * @param methodNames	The list of method names
     * @param shownNames	How to label each method
     */
    public static void showList(Class owner, String[] methodNames, String[] shownNames)
    {
        if (methodNames.length != shownNames.length) {
			throw new Error("The lists given to Test.showList() MUST be of the same length!");
		}
        
        MethodMenu t = new MethodMenu(owner, methodNames, shownNames);
        // Wait for the dialog to be done:
        while (!t.done)
        {
            try { Thread.sleep(500); }
            catch (InterruptedException er) { }
        }
    }
}
