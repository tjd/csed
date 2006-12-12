package javaintro;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Scanner;

/*
 * 
 * Listfile.java
 * 
 * Demonstrates basic reading a text file using a Scanner object, and writing
 * a text file using a PrintWriter. Also shows an example of using String.format, and
 * the syntax for catching multiple exceptions.
 * 
 */

public class Listfile {

	public static void main(String[] args) {
		String fname = "javaintro/Listfile.java";
		String outfilename = "javaintro/out.dat";

		try {
			// use Scanner to read the input text file
			Scanner infile = new Scanner(new File(fname));

			// next 3 commands set up a text file writer
			FileWriter fw = new FileWriter(outfilename);
			BufferedWriter bw = new BufferedWriter(fw);
			PrintWriter outfile = new PrintWriter(bw);

			int count = 0;
			while (infile.hasNextLine()) {
				++count;
				String line = infile.nextLine();

				// "%3d" prints a decimal number right-justified in 3 spaces
				String result = String.format("%3d: %s\n", count, line);
				System.out.print(result);
				outfile.print(result);
			}
			outfile.close();
		} catch (FileNotFoundException e) {
			System.out.printf("The file \"%s\" was not found.", fname);
		} catch (IOException e) {
			System.out.printf("Unable to create \"%s\"", outfilename);
		}

	}

}
