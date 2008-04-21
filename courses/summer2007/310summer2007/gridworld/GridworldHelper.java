package gridworld;

import java.util.Vector;

import util.EasyInput;

public class GridworldHelper {

	public static final char WALL = 'w';

	public static final char EMPTY = '.';

	public static char[][] getMap(String fname) {
		String map = EasyInput.readfile(fname);
		String[] rows = map.split("\n");
		char[][] result = new char[rows[0].length()][rows.length];
		for (int i = 0; i < rows.length; ++i) {
			String row = rows[i];
			for (int j = 0; j < row.length(); ++j) {
				result[i][j] = row.charAt(j);
			}
		}
		return result;
	}

	public static Vector<Character> charToVec(char[] arr) {
		Vector<Character> result = new Vector<Character>();
		result.setSize(arr.length);
		for(int i = 0; i < arr.length; ++i) {
			result.set(i, arr[i]);
		}
		return result;
	}
	
	public static Vector<Vector<Character>> toVector(char[][] map) {
		Vector<Vector<Character>> result = new Vector<Vector<Character>>();
		for (int i = 0; i < map.length; ++i) {
			result.add(charToVec(map[i]));
		}
		return result;	
	}

	public static void printMap(Vector<Vector<Character>> map) {
		for (int i = 0; i < map.size(); ++i) {
			for (int j = 0; j < map.get(i).size(); ++j) {
				System.out.print(map.get(i).get(j));
			}
			System.out.println();
		}
	}
	
	public static void printMap(char[][] map) {
		printMap(toVector(map));
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		EasyInput.printCwd();
		char[][] map = getMap("/home/toby/workspace/scratch/mazeout.txt");
		printMap(map);
	}

}
