package csmedia;

/*
 * Created on Jun 9, 2004
 *
 * imported on July 11, 2004 from mediaplayer8 version
 */
/**
 * @author Robert
 * 
 */
public interface MusicFilePlayerInterface {

	/**
	 * Known classes implementing this interface: MusicFilePlayerBase
	 */

	void play();

	void restart();

	void stop();

	/**
	 * The parameter for volume is a double, with 0.0 being quietest, and 1.0
	 * being the loudest amplitude the platform can handle without distortion.
	 */
	void setVolume(double d);
}