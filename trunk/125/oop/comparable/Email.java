package oop.comparable;

public class Email implements Comparable<Object> {

	private String username;
	private String domain;

	public Email(String username, String domain) {
		this.username = username.trim();
		this.domain = domain.trim();
	}
	
	public Email(String email) {
		int atLoc = email.indexOf("@");
		if (atLoc < 0) {
			throw new Error("invalid email address");
		} else {
			this.username = email.substring(0, atLoc);
			this.domain = email.substring(atLoc + 1);
		}
	}
	
	
	public int compareTo(Object s) {
		Email other = (Email) s;
		return this.getDomain().compareTo(other.getDomain());
//		return this.getUsername().compareTo(other.getUsername());
	}
	
	public String getDomain() {
		return domain;
	}

	public String getUsername() {
		return username;
	}
	
	public String toHTML() {
		return String.format("<a href = \"mailto:%s\">%s</a>", this, this);
	}
	
	public String toString() {
		return username + "@" + domain;
	}
	
	public static void main(String[] args) {
//		assert 1 == 2;
		Email email1 = new Email("tjd@sfu.ca");
		assert email1.getUsername().equals("tjd");
		assert email1.getDomain().equals("sfu.ca");
		assert "tjd@sfu.ca".equals(email1.toString());
		assert email1.toHTML().equals("<a href = \"mailto:tjd@sfu.ca\">tjd@sfu.ca</a>");
		System.out.printf("all tests passed");
	}
}
