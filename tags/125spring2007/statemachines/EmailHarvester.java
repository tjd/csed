package statemachines;

/* 
 * Problem: Write a program that will read a text file or web page, and print
 * out a list of all the email address that occur on it in alphabetical order
 * (by username).
 * 
 */

import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.regex.Matcher;

public class EmailHarvester {

	public static void main(String[] args) {
		// get the text to process
//		String url = "http://www.sfu.ca/english/faculty.htm";
		String url = "http://www.sfu.ca/economics/faculty/index.html";
//		String url = "http://www.cs.sfu.ca/people/Faculty/";
		String text = getWebPageAtUrl(url);

		// extract all the email addresses
		ArrayList<Email> allEmails = getAllEmails(text);
		System.out.printf("\n(%s email addresses extracted)", allEmails.size());

		// sort them
		TreeMap<String, Email> map = sortByUsername(allEmails);
		System.out.println("(email addresses sorted)");

		// get rid of duplicates
		Set<Email> set = removeDuplicates(map.values());

		// print the results
		print(set.toArray());

		System.out.printf("\ndone (%s unique email addresses)", set.size());
	}

	// Opens the given file and returns its contents as a string.
	public static String getFile(String fname) {
		System.out.printf("Processing %s ...", fname);
		String text = util.EasyInput.fileToString(fname);
		System.out.println(" got file.");
		return text;
	}

	// Opens the given web page and returns its contents as a string.
	public static String getWebPageAtUrl(String url) {
		System.out.printf("Processing %s ...", url);
		String text = util.EasyInput.getWebPage(url);
		System.out.println(" got URL.");
		return text;
	}

	// A TreeMap stores (key, value) pairs, and keeps the pairs
	// in order by key.
	public static TreeMap<String, Email> sortByUsername(ArrayList<Email> lst) {
		TreeMap<String, Email> result = new TreeMap<String, Email>();
		for (Email email : lst) {
			result.put(email.getUsername(), email);
		}
		return result;
	}

	// Removes all the duplicates in c by converting it to a TreeSet.
	public static TreeSet<Email> removeDuplicates(Collection c) {
		return new TreeSet<Email>(c);
	}

	// Returns an ArrayList of all the emails in string s.
	public static ArrayList<Email> getAllEmails(String s) {
		ArrayList<Email> result = new ArrayList<Email>();
		Matcher m = Email.emailPattern.matcher(s);
		while (m.find()) {
			Email email = new Email(m.group(1), m.group(2), m.group(3));
			result.add(email);
		}
		return result;
	}

	// Replaces email addresses in string s with human-readable string
	public static String replaceAllEmails(String s) {
		return s.replaceAll(Email.emailPattern.pattern(), "$1_AT_$2_DOT_$3");
	}

	public static void print(ArrayList<Email> lst) {
		for (Email e : lst) {
			System.out.println(e);
		}
	}

	public static void print(Object[] arr) {
		for (Object e : arr) {
			System.out.println(e);
		}
	}

}