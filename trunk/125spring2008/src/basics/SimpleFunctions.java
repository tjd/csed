package basics;

public class SimpleFunctions {

	public static void main(String[] args) {
		System.out.printf("The area of a circle with radis 4.5 is %.1f",
				areaOfCircle(4.5));
		welcome();
	}

	public static double areaOfCircle(double radius) {
		return Math.PI * radius * radius;
	}

	public static void welcome() {
		System.out.println("Welcome to Surrey!");
	}	
    
}
