package statemachines;

public class Parser {

	private final static String SEP_REGEX = "\\s+";

	private final static String COLOR_REGEX = "color\\s+\\d\\d?\\d?\\s+\\d\\d?\\d?\\s+\\d\\d?\\d?";

	public static void main(String[] args) {
		String line = "   color   12 34 56  ";
		line = line.trim();
		
		if (line.matches(COLOR_REGEX)) {
			System.out.printf("'%s' is a valid color statement\n\n", line);
		}
		
		String[] tokens = line.split(SEP_REGEX);

		for (String token : tokens) {
			System.out.printf("'%s'\n", token);
		}

		System.out.printf("\ndone\n");
	}

}
