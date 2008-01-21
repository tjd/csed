package textfiles;

/* 
 * Prints the contents of a given text file on the screen.
 * Throws away the file not found exception.
 * 
 */


import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class PrintFile2 {

	public static void main(String[] args) {
		try {
			Scanner fs = new Scanner(new File("fileprinter/PrintFile2.java"));
			int num = 1;
			while (fs.hasNext()) {
				String line = fs.nextLine();
				System.out.printf("%s: %s\n", num, line);
				++num;
			}
		} catch (FileNotFoundException e) {
			System.out.println("Oops: I couldn't find the file.");
			// e.printStackTrace();
		}
	}

}
