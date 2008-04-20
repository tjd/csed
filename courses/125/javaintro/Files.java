package javaintro;

/*
 * 
 * Files.java
 * 
 * Demonstrates the use of EasyInput.java, a locally developed utility class for Java.
 * 
 * Also shows the Java "for ... in" statement that is new in Java 1.5.
 * 
 */

public class Files {

	public static void main(String[] args) {

		// print the current working directory
		System.out.printf("pwd = %s\n", EasyInput.getcwd());

		// get a list of files in the current working directory,
		// and store their names as an array of strings
		String[] files = EasyInput.listdir();

		// print all the files in the list
		for (String f : files) {
			System.out.printf("%s\n", f);
		}

		String fname = EasyInput.chooseFile();
		System.out.printf("You chose \"%s\"", fname);
	}

}
