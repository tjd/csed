package statemachines;

import java.util.regex.Pattern;

public class Email implements Comparable {

	private String username, place, extension;

	// Matches an email address of the form "username@place.ext". Any amount of
	// whitespace is allowed at the beginning or end. The username and
	// extension must each be one or more alphanumeric characters.
	// The place contains alphanumeric characters, including possible "."
	// characters. The brackets divide the address into groups for
	// easier parsing.
	final static public Pattern emailPattern = Pattern
			.compile("(\\w+)@([\\w.]+)\\.(\\w+)");

	public Email(String username, String place, String ext) {
		this.username = username;
		this.place = place;
		this.extension = ext;
	}

	/**
	 * @return Returns the extension.
	 */
	public String getExtension() {
		return this.extension;
	}

	/**
	 * @return Returns the place.
	 */
	public String getPlace() {
		return this.place;
	}

	/**
	 * @return Returns the username.
	 */
	public String getUsername() {
		return this.username;
	}

	@Override
	public boolean equals(Object o) {
		Email other = (Email) o;
		return this.username.equals(other.username)
				&& this.place.equals(other.place)
				&& this.extension.equals(other.extension);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(Object o) {
		Email other = (Email) o;
		return this.username.compareTo(other.username);
	}

	public String toHTML() {
		return this.toHTML(this.toString());
	}

	public String toHTML(String text) {
		return "<a href = \"mailto:" + this.toString() + "\">" + text + "</a>";
	}

	@Override
	public String toString() {
		return this.username + "@" + this.place + "." + this.extension;
	}

	public static void main(String[] args) {
		Email email1 = new Email("tjd", "sfu", "ca");
		System.out.println("Email 1: " + email1);
		System.out.println("Email 1: " + email1.toHTML());
	}

}