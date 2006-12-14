/*
 * Created on June 25, 2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package csspeech;

import javax.swing.JOptionPane;


/**
 * @author I-Ling
 */
public class TestTextSpeech {
    /**
     *  
     */
    public TestTextSpeech() {
        super();
        // TODO Auto-generated constructor stub
    }

    public static void main(String args[]) {
        Speaker s = new Speaker();
        String prompt = "Please enter what you want me to say (enter to quit): ";
        String sentence = JOptionPane.showInputDialog(prompt);

        while (sentence != null && sentence.length() > 0) {
            s.speak(sentence);
            sentence = JOptionPane.showInputDialog(prompt);
        } // while

        s.Deallocate();
        System.out.println("Done!");
    }
}