package basics;

public class StringBasics {

	public static void main(String[] args) {
		String name = "Goodtime Charlie";
		int len = name.length();
		System.out.printf("The string \"%s\" is %s characters long.\n", name,
				len);
		
		String gossip = name + " is quite the elbow-lifter.";
		System.out.printf("%s\n", gossip);		
	}

}
