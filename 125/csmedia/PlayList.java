/*
 * Created on Jun 15, 2004
 *
 * imported on July 11, 2004 from mediaplayer8 version
 */
package csmedia;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Vector;

import javax.swing.JOptionPane;

/**
 * @author Robert
 * 
 */
public class PlayList extends Vector {

	private PrintWriter outFile;

	private BufferedReader inFile;

	public PlayList() {
		super();
	}

	public void addToTop(String name, String location) {
		addToTop(new PlayListElement(name, location));
	}

	public void addToTop(Object o) {
		if (o instanceof PlayListElement) {
			add(0, o);
		} else {
			System.out.println("The program tried to add an object other than"
					+ "a PlayListElement to the PlayList.");
		}
	}

	public void toTop(int i) {
		if (i < 0 || i >= this.size()) {
			return;
		}
		add(0, remove(i));
	}

	public void toLast(int i) {
		if (i < 0 || i >= this.size()) {
			return;
		}
		add(remove(i));
	}

	public void delete(int i) {
		if (i < 0 || i >= this.size()) {
			return;
		}
		remove(i);
	}

	public void save() {
		try {
			// tries to write a playlist.log file
			outFile = new PrintWriter(new FileOutputStream("playlist.log",
					false));
		} catch (FileNotFoundException exc) {
			JOptionPane.showMessageDialog(null,
					"The program threw a FileNotFoundException "
							+ "when it tried to open playlist.log",
					"FileNotFoundException", JOptionPane.PLAIN_MESSAGE);
			/*
			 * error message pops up if playlist.log file could not be written
			 */
		}

		for (int i = 0; i < this.size(); i++) {
			PlayListElement currentEl = (PlayListElement) this.elementAt(i);
			outFile.println(currentEl.getName());
			outFile.println(currentEl.getLocation());
		}
		outFile.close();

	}

	public void restore() {
		try {
			// tries to open a playlist.log file
			inFile = new BufferedReader(new FileReader("playlist.log"));
			// try {
			// while (true) {
			// String name = inFile.readLine();
			// String location = inFile.readLine();
			// if (name == null) break;
			// this.addToTop(name, location);
			// }
			// } catch (NullPointerException exc) {
			// }
			while (true) {
				String name = inFile.readLine();
				String location = inFile.readLine();
				if (name == null) {
					break;
				}
				this.addToTop(name, location);
			}
			inFile.close();
		} catch (IOException exc) {
		}

	}
}