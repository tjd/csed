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
		DiaryEntry entry = new DiaryEntry(body, this.author);
		entries.add(entry);
	}

	public void printAll() {
		String sep = "---------------------------------------\n";
		for (int i = 0; i < entries.size(); ++i) {
			System.out.printf("%s%s\n%s", sep, entries.get(i), sep);
		}
	}

	public DiaryEntry getMostRecentEntry() {
		return entries.get(entries.size() - 1);
	}

	public static void main(String[] args) {
		DiaryAuthor author = new DiaryAuthor("Toby", "Donaldson", "tjd@sfu.ca");
		Diary diary = new Diary(author);
		diary.addEntry("I like cheese. It's really good.");
		diary.addEntry("Ever wonder who invented the shoe? What a genius!");
		diary.addEntry("If music be the food of love, "
				+ "then I'll have a Whopper and large fries.");
		diary.printAll();

	}

}
