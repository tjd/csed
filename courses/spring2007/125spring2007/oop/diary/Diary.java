package oop.diary;

import java.util.ArrayList;

public class Diary {

	private ArrayList<DiaryEntry> entries;

	private DiaryAuthor author;

	public Diary(DiaryAuthor author) {
		entries = new ArrayList<DiaryEntry>();
		this.author = author;
	}

	public void addEntry(String body) {
		addEntry(new DiaryEntry(body, this.author));
	}
	
	public void addEntry(DiaryEntry e) {
		entries.add(e);
	}

	public void printAll() {
		String sep = "---------------------------------------\n";
		for (DiaryEntry entry : entries) {
			System.out.printf("%s%s\n%s", sep, entry, sep);
		}
	}

	public DiaryEntry getMostRecentEntry() {
		return entries.get(entries.size() - 1);
	}

	public static void main(String[] args) {
		DiaryAuthor author = new DiaryAuthor("Toby", "Donaldson", 
					"tjd@sfu.ca");
		Diary diary = new Diary(author);
		
		diary.addEntry("I like cheese. It's really good.");
		diary.addEntry("Ever wonder who invented the shoe? What a genius!");
		diary.addEntry("If music be the food of love, "
				+ "then I'll have a Whopper and large fries.");
		
		DiaryEntry entry = new DiaryEntry("Hi!", author);
		entry.addKeyword("introduction");
		entry.addKeyword("hello");
		diary.addEntry(entry);
		
		diary.printAll();
	}

}
