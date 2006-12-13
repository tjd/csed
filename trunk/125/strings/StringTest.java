package strings;

import java.util.Scanner;

public class StringTest {

	public static void main(String[] args) {
		Scanner sc = new Scanner(System.in);
		System.out.print("What is your age? --> ");
		int age = sc.nextInt();
		String s = "" + age; // convert age to a String
		
		// print each character of s on its own line
		for(int i = 0; i < s.length(); ++i) {
			char c = s.charAt(i);
			System.out.println(c);
		}		
	}

}
