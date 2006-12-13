package oop.diary;

import oop.comparable.Email;

public class DiaryAuthor {

	private String firstName;

	private String lastName;

	private Email email;

	public DiaryAuthor(String first, String last, String email) {
		this.firstName = first;
		this.lastName = last;
		this.email = new Email(email);
	}

	public String getEmail() {
		return email.toString();
	}

	public String getUsername() {
		return email.getUsername();
	}

	public String getFirstName() {
		return firstName;
	}

	public String getLastName() {
		return lastName;
	}

	public String getFullName() {
		return getFirstName() + " " + getLastName();
	}

	public String toString() {
		return String.format("%s (%s)", getFullName(), getEmail());
	}

	public static void main(String[] args) {
		// assert 1 == 2;
		DiaryAuthor author = new DiaryAuthor("Toby", "Donaldson", "tjd@sfu.ca");
		assert author.getEmail().equals("tjd@sfu.ca");
		assert author.getUsername().equals("tjd");
		assert author.getFirstName().equals("Toby");
		assert author.getLastName().equals("Donaldson");
		assert author.getFullName().equals("Toby Donaldson") : author
				.getFullName();
		assert author.toString().equals("Toby Donaldson (tjd@sfu.ca)") : author
				.toString();
		System.out.println("all tests passed");
	}
}
