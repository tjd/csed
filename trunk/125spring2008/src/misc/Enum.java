package misc;

public class Enum {

	enum Season {
		WINTER, SPRING, SUMMER, FALL
	}

	public static void main(String[] args) {
		Season a = Season.SPRING;
		Season b = Season.FALL;
		
		if (a.equals(b)) {
			System.out.printf("same");
		} else {
			System.out.printf("different");
		}
	}

}
