package oop.diary;

import java.util.ArrayList;
import java.util.Date;


public class DiaryEntry {

	private String body;
	private Date postingDate;
	private DiaryAuthor author;
	private ArrayList<String> keywords;
	
	public DiaryEntry(String body, DiaryAuthor author) {
		this.body = body;
		this.postingDate = new Date(); 
		this.author = author;
		this.keywords = new ArrayList<String>();
	}
	
	public void addKeyword(String word) {
		keywords.add(word);
	}
	
	public boolean hasKeyword(String word) {
		return keywords.contains(word);
	}

	public boolean bodyContains(String word) {
		return body.contains(word);
	}
	
	public DiaryAuthor getAuthor() {
		return author;
	}

	public String getBody() {
		return body;
	}

	public ArrayList<String> getKeywords() {
		return keywords;
	}

	public Date getPostingDate() {
		return postingDate;
	}

	public String toString() {
		return this.body + "\nby " + author + ", " + postingDate + "\nKeywords: " + keywords;
	}
	
	public static void main(String[] args) {
		DiaryAuthor author = new DiaryAuthor("Toby", "Donaldson", "tjd@sfu.ca");
		String body = "My cat Newton is so cute. I demand that you look at these pictures!";
		DiaryEntry entry = new DiaryEntry(body, author);
		entry.addKeyword("cat");
		entry.addKeyword("cute");
		System.out.println(entry);
	}
	
}
