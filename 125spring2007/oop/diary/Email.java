package oop.diary;

public class Email implements Comparable<Email> {

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

	public int compareTo(Email other) {
		return this.getDomain().compareTo(other.getDomain());
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

	@Override
	public String toString() {
		return username + "@" + domain;
	}

	public static void main(String[] args) {
		util.Util.ensureAssertionsEnabled();
		util.Util.ensureAssertionsEnabled();
		Email email1 = new Email("tjd@sfu.ca");
		assert email1.getUsername().equals("tjd");
		assert email1.getDomain().equals("sfu.ca");
		assert "tjd@sfu.ca".equals(email1.toString());
		assert email1.toHTML().equals(
				"<a href = \"mailto:tjd@sfu.ca\">tjd@sfu.ca</a>");
		System.out.printf("all tests passed");
	}
}
