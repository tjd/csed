package csimage.util;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Iterator;

public class EasyOutput {

	/*
	 * Writes string s to the file with the given name. If the file
	 * already exists it is overwritten.
	 * 
	 * @param path
	 *    path name of the file to write to
	 *    
	 * @param s
	 *    the string to write to the file
	 */ 
	public static void writeToFile(String path, String s) {
		try {
			// open an output stream for printing
			PrintStream out = new PrintStream(new FileOutputStream(path));
			out.print(s);
			out.close();
		} catch (IOException e) {
			System.err
					.println("IOException in EasyOutput.writeToFile(String path, String s)!");
			System.exit(-1);
		}
	}

	/*
	 * Writes string s to the file with the given name. If the file
	 * already exists it is overwritten.
	 * 
	 * Note: Java arrays do not implement the Iterable interface. 
	 * 
	 * @param c
	 *    an iterable container with elements of type T
	 *    
	 * @param sep
	 *    the string to seperate the items
	 *    
	 * @return
	 *     a string contain all the items of c (converted to string by calling their
	 *     .toString() methods) in order, and seperated by string sep; note that sep
	 *     is not added to the beginning or end of the string --- the seperation string
	 *     only occurs between elements   
	 */ 
	public static <T> String join(Iterable<T> c, String sep) {
		Iterator<T> iter = c.iterator();
		if (!iter.hasNext()) { // c is empty
			return "";
		} else { // c has at least 1 item
			StringBuilder result = new StringBuilder();
			result.append(iter.next());
			while (iter.hasNext()) {
				result.append(sep + iter.next());
			}
			return result.toString();
		}
	}

	/*
	 * Writes each element of container c to the file with the given name. Each
	 * element is seperated by a string sep, excluding the last element. If the
	 * file already exists it is overwritten,
	 * 
	 * Note: Java arrays do not implement the Iterable interface. 
	 * 
	 * @param path
	 *    path name of the file to write to
	 * 
	 * @param c
	 *    an iterable container with elements of type T
	 *    
	 * @param sep
	 *    the string to seperate the items
	 *  
	 */ 
	public static <T> void writeToFile(String path, Iterable<T> c, String sep) {
		writeToFile(path, join(c, sep));
	}

	/*
	 * Writes each element of container c to the file with the given name. Each
	 * element is seperated by a "\n". If the file already exists it is overwritten.
	 * 
	 * Note: Java arrays do not implement the Iterable interface. 
	 * 
	 * @param path
	 *    path name of the file to write to
	 * 
	 * @param c
	 *    an iterable container with elements of type T
	 *  
	 */ 
	public static <T> void writeToFile(String path, Iterable<T> c) {
		writeToFile(path, c, "\n");
	}

	public static void main(String[] args) {
		ArrayList<Integer> lst = new ArrayList<Integer>();
		lst.add(1); lst.add(2); lst.add(3); lst.add(4); lst.add(5);
		System.out.println(join(lst, ", "));
		
		EasyInput.printCwd();
		writeToFile("lst.txt", lst);
	}

}
