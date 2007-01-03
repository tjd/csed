package csmedia;

/*
 * 
 * This is a helper class that makes it easy to play MP3 and MPEG files. It
 * implements MusicFilePlayerInterface, found in this same package. It relies on
 * the Java Media Framework, which can be downloaded for free from
 * http://java.sun.com/products/java-media/jmf/2.1.1/download.html . You will
 * have to download and install the correct version of the JMF for your
 * platform, as the cross-platform version does not support MP3 or MPEG files.
 * 
 * Notice that there is a debugging flag in the class. If you want debugging
 * messages to be printed, set DEBUG to true, otherwise set it to false.
 * 
 * imported on July 11, 2004 from mediaplayer8 version
 */

import java.awt.FileDialog;
import java.io.File;
import java.sql.Time;

import javax.swing.JOptionPane;

//import com.sun.media.MediaPlayer;

public class MusicFilePlayerBase implements MusicFilePlayerInterface,
        ControllerListener {

    // when DEBUG is true, debugging messages will be printed
    public static boolean DEBUG = true;

    private String filename;

    private Player m_Player;

    private File file;

    private FileDialog fileDialog;

    /*
     * Constructor for a MediaPlayerHelper object. If the String parameter is
     * null this method does nothing. Make sure to set the volume after calling
     * this constructor. Note that the parameter MusicFileName must consist of
     * the directory concatenated with the name of the individual file
     * (including extension).
     */
    public MusicFilePlayerBase(String musicFileName) {
        if (musicFileName == null) {
			return;
		}
        dbg(musicFileName);
        m_Player = createPlayerforFile(musicFileName);
        if (m_Player == null) {
			return;
		}
        dbg("Instantiated MediaPlayer...");
        dbg("Running constructor...");
        setVolume(0.5);
        m_Player.addControllerListener(this);
    }

    /*
     * This method starts playing a music file that is already loaded. It starts
     * from the beginning of the file, or from where it left off. It does not go
     * back to the beginning unless it reaches the end of the music file.
     * 
     * The method controllerUpdate in this same class will detect when the end
     * of the media file is reached and will move the player back to the
     * beginning when that happens.
     */
    public void play() {

        // Record the current position of m_Player in its media file.
        double position = m_Player.getMediaTime().getSeconds();

        m_Player.start();

        // Wait for m_Player to actually start up before setting position.
        // Otherwise m_Player will reset the position to zero when it starts up.
        waitMilliseconds(5);
        m_Player.setMediaTime(new Time(position + 0.01));
    }

    public void restart() {

        //		The m_Player must be realized so that its position can be changed.
        ensureRealized(m_Player);
        m_Player.setMediaTime(new Time(0.0));
    }

    /*
     * This method will stop the player without restarting it. If you want to
     * restart the player, invoke the restart() method. Calling stop() while the
     * Player is stopped will do nothing.
     */
    public void stop() {
        m_Player.stop();
    }

    /*
     * The parameter for volume is a double, with 0.0 being quietest, and 1.0
     * being the loudest amplitude the platform can make without distortion. The
     * default is 0.5 .
     */
    public void setVolume(double d) {
        boolean wasPlayerStarted = isPlayerPlaying();
        ensureRealized(m_Player);
        GainControl gainControl = m_Player.getGainControl();
        dbg("gainControl = " + gainControl);
        dbg("gainControl.getLevel() = " + gainControl.getLevel());
        gainControl.setLevel((float) d);
        if (wasPlayerStarted) {
            play();
        }
    }

    public double getVolume() {
        return (float) m_Player.getGainControl().getLevel();
    }

    public boolean isPlayerPlaying() {
        return (m_Player.getState() == Controller.Started);
    }

    /*
     * Automatically called when m_Player reaches the end of a media file. It
     * requires the method addControllerListener() to be called first.
     */
    public void controllerUpdate(ControllerEvent e) {
        if (e instanceof EndOfMediaEvent) {
            dbg("Caught an EndOfMediaEvent");
            restart();
        }
    }

    //
    // Helper methods:
    //

    /*
     * Some methods of the JMF requre the Player to be realized. In other words,
     * all the resources the Player needs must be loaded and available, but the
     * Player must not be actually playing. This method checks which state a
     * Player is in, and modifies its state until it is realized.
     */
    private void ensureRealized(Player player) {
        int state = player.getState();
        while (state != Controller.Realized) {
            if (state == Controller.Unrealized) {
                player.realize();
            } else if (state == Controller.Realizing) {
                waitMilliseconds(100);
            } else if (state == Controller.Started) {
                player.stop();
            } else {
                player.deallocate();
            }
            state = player.getState();
        }
        dbg("End of method ensureRealized()");
    }

    /*
     * Create a Player for the given file. Note that the parameter MusicFileName
     * must consist of the directory concatenated with the name of the
     * individual file (including extension).
     */
    private Player createPlayerforFile(String musicFileName) {
        file = new File(musicFileName);
        try {
            return Manager.createPlayer(file.toURL());
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null, "Please make "
                    + "sure to choose an MP3 or MPEG file.",
                                          "This file cannot be played.",
                                          JOptionPane.PLAIN_MESSAGE);
            dbg("Exception caught: unable to create player.");
            return null;
        }
    }

    /*
     * Make the current thread sleep for the specified number of milliseconds.
     * This is used to allow other parts of the program to load fully before the
     * main program proceeds.
     */
    private void waitMilliseconds(int n) {
        try {
            Thread.sleep(n);
            dbg("Waiting...");
        } catch (InterruptedException e) {
            System.out.println("Thread interrupted while sleeping");
        }
    }

    private void dbg(Object o) {
        if (DEBUG) {
            System.out.println(o);
        }
    }

}