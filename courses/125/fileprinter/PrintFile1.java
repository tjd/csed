package fileprinter;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class PrintFile1 {

	public static void main(String[] args) throws FileNotFoundException {
		// EasyInput.printCwd();
		// System.out.println(Arrays.toString(EasyInput.listdir()));
		Scanner fs = new Scanner(new File("fileprinter/PrintFile1.java"));
		int num = 1;
		while (fs.hasNext()) {
			String line = fs.nextLine();
			System.out.printf("%s: %s\n", num, line);
			++num;
		}
	}

}
