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

public class PrintFile4 {
	public static void main(String[] args) {
		String fname = util.EasyInput.chooseFile();
		try {
			Scanner fs = new Scanner(new File(fname));
			int num = 1;
			while (fs.hasNext()) {
				String line = fs.nextLine();
				System.out.printf("%03d: %s\n", num, line);
				++num;
			}
		} catch (FileNotFoundException e) {
			System.out.printf("Oops: I couldn't find a file named \"%s\"", fname);
			// e.printStackTrace();
		}
	}
}
