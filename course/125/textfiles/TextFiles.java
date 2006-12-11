package textfiles;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Scanner;

public class TextFiles {

	public static void main(String[] args) {
		// change BASE_PATH to be the path to the folder on your computer where you want
		// to store your text files
		final String BASE_PATH = "/Users/toby/Documents/workspace/cmpt125/textfiles/";
		String[] lines = { "This is a test.", "Please stop reading.", "Right.",
				"Now." };
		writeFile(BASE_PATH + "story2.txt", lines);
		printFile(BASE_PATH + "story2.txt");
	}

	public static void writeFile(String path, String[] lines) {
		try {
			File file = new File(path);
			FileWriter fw = new FileWriter(file);
			BufferedWriter bw = new BufferedWriter(fw);
			PrintWriter output = new PrintWriter(bw);

			for (int i = 0; i < lines.length; ++i) {
				output.printf(" - %s\n", lines[i]);
			}
			output.close();
		} catch (IOException e) {
			System.out.print(e);
		}
	}

	public static void printFile(String path) {
		try {
			File file = new File(path);
			Scanner input = new Scanner(file);
			int lineNum = 1;
			while (input.hasNext()) {
				String line = input.nextLine();
				System.out.printf("%s: %s\n", lineNum, line);
				++lineNum;
			}
		} catch (IOException e) {
			System.out.print(e);
		}
	}

}
