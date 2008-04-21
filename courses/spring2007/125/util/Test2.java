package util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Random;

public class Test2 {

	public static void main(String[] args) {
		String[] entries = EasyInput
				.listdir("C:\\Documents and Settings\\tjd\\Desktop\\spring2007\\275");
		ArrayList<String> arr = new ArrayList<String>();
		for (String entry : entries) {
			if (entry.endsWith(".xls")) {
				arr.add(entry);
			}
		}

		System.out.println(Arrays.toString(entries));
		System.out.println(arr);

		Random rnd = new Random();
		int r = rnd.nextInt(arr.size());
		String fname = arr.get(r);
		System.out.println(fname);
	}

}
