package textfiles;

/* 
 * Prints the contents of a given text file on the screen.
 * The line numbers are padded with leading 0s so that they
 * all line up evenly.
 * 
 */

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class PrintFile3 {
	public static void main(String[] args) {
		String fname = util.EasyInput.chooseFile();
		try {
			Scanner fs = new Scanner(new File(fname));
			int num = 1;
			while (fs.hasNext()) {
				String line = fs.nextLine();
				System.out.printf("%s: %s\n", padNum(num, 2), line);
				++num;
			}
		} catch (FileNotFoundException e) {
			System.out.println("Oops: I couldn't find the file.");
			// e.printStackTrace();
		}
	}

	public static String padNum(int n, int width) {
		return padNum(n, width, '0');
	}

	// Adds enough 0s to the start of n to make its length equal to width.
	// If n is already longer than width, then return n (as a String)
	// unchanged.
	public static String padNum(int n, int width, char padChar) {
		String s = "" + n;
		int len = s.length();
		if (len >= width) {
			return s;
		} else {
			return fill(width - len, padChar) + s;
		}
	}

	// returns a string of length n consisting of only c characters
	public static String fill(int n, char c) {
		String result = "";
		for (int i = 0; i < n; ++i) {
			result += c;
		}
		return result;
	}

}
