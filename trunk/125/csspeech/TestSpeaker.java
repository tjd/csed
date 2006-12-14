package csspeech;

import csimage.util.EasyInput;

/*
 * Created on June 22, 2004
 * 
 * last modified: June 27, 2004
 */

public class TestSpeaker extends EasyInput {

    public static void main(String args[]) {
        Speaker s = new Speaker();
        String sentence = input("Please enter what you want me to say (enter to quit): ");
        while (sentence.length() > 0) {
            s.speak(sentence);
            sentence = input("Please enter what you want me to say (enter to quit): ");
        } // while

        System.out.println("Done!");
    }
}