package csspeech;

import java.io.IOException;
import java.util.*;
import javax.speech.*;
import javax.speech.synthesis.*;

// Adapted from "Simply Java Programming"
// by Deitel and Deitel, page 820
// AHD Sat 5 June 2004, 6:48 PT
//
// last modified: June 27, 2004

/**
 * This ia a class which provides the conversion of a string of text to speech
 * (TTS) using the FreeTTS Speech API.
 */
public class Speaker implements Speaks {
    // Synthesizer to speak text
    private Synthesizer speechSynthesizer;

    // no-argument constructor
    public Speaker() {
        // initialize Synthesizer
        try {
            // create SynthesizerModeDesc for FreeTTS synthesizer.
            SynthesizerModeDesc descriptor = new SynthesizerModeDesc(
                                                                     "Unlimited domain FreeTTS Speech Synthesizer "
                                                                             + "from Sun Labs",
                                                                     null,
                                                                     Locale.US,
                                                                     Boolean.FALSE,
                                                                     null);

            // create a Synthesizer
            speechSynthesizer = Central.createSynthesizer(descriptor);

            // Synthesizer created successfully
            if (speechSynthesizer != null) {
                // prepare synthesizer to speak
                speechSynthesizer.allocate();
                speechSynthesizer.resume();

                // get synthesizer properties
                SynthesizerProperties properties = speechSynthesizer
                        .getSynthesizerProperties();

                // set up speaking rate
                properties.setSpeakingRate(100.0f);

            } // end if
            else {
                throw new Error("Synthesizer creation failed.");
            }

        } // end try

        catch (Exception e) {
            e.printStackTrace();
        }

    } // end constructor

    /**
     * Converts a string of text to speech.
     * 
     * @param textToSpeak
     *            the text to be spoken
     */
    public void speak(String textToSpeak) {
        speechSynthesizer.speakPlainText(textToSpeak, null);
    }

    /**
     * Deallocates speechSynthesizer object.
     */
    public void Deallocate() {
        try {
            //http://pixel.recoil.org/java/jspeechlib/ResourceManagement.html
            // Wait till speaking is done
            speechSynthesizer.waitEngineState(Synthesizer.QUEUE_EMPTY);
            speechSynthesizer.deallocate();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // finalizer method is called automatically when this object is
    // garabage-collected by Java
    protected void finalize() throws IOException {
        Deallocate();
    }

    // 	/**
    //	 main method to test the class
    //	*/
    // 	public static void main(String args[]) {
    //		Speaker s = new Speaker();
    //		char ans = 'y';
    //		while (ans == 'y') {
    //			System.out.println("Please enter some text...");
    //			String sentence = SpeakerHelper.readLine();
    //	    	s.speak(sentence);
    //	    	System.out.println("Continue? y/n");
    //	    	ans = SpeakerHelper.readLineNonwhiteChar();
    //	    }
    //                        
    //        s.Deallocate();
    //	}

} // end class Speaker

