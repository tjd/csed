/*
 * Created on Jun 15, 2004
 *
 * imported on July 11, 2004 from mediaplayer8 version
 */
package csmedia;

/**
 * @author Robert
 * 
 */
public class PlayListElement {

	private String name;

	private String location;

	public PlayListElement(String name, String location) {
		this.name = name;
		this.location = location;
	}

	/**
	 * @return
	 */
	public String getLocation() {
		return location;
	}

	/**
	 * @return
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param string
	 */
	public void setLocation(String s) {
		location = s;
	}

	/**
	 * @param string
	 */
	public void setName(String s) {
		name = s;
	}

}