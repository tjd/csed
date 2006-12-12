package collections;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Scanner;

public class ReadFile {

	public static void main(String[] args) {
		try {
			File file = new File("/Users/toby/Desktop/scratch/is5.txt");
			Scanner sc = new Scanner(file);

			ArrayList<String> lst = new ArrayList<String>();

			while (sc.hasNextLine()) {
				String line = sc.nextLine();
				lst.add(line);
			}

			System.out.printf("There are %s lines in the file\n", lst.size());
			System.out.printf("first: \"%s\"\n", lst.get(0));
			System.out.printf("last: \"%s\"\n", lst.get(lst.size() - 1));

			// print the entire contents of the file
			for(int i = 0; i < lst.size(); ++i) {
				System.out.println(lst.get(i));
			}

		} catch (IOException e) {
			System.out.println("I/O error: " + e);
		}
	}
}


