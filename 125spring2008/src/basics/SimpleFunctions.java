package basics;

public class SimpleFunctions {

	public static void main(String[] args) {
		welcome();
		System.out.printf("The area of a circle with radis 4.5 is %.1f",
				areaOfCircle(4.5));		
		
		int m = 32;
		double accel = 3.4;
		double f = force(32, 3.4);
		System.out.printf("The force = %.2f\n", f);
	}

	public static double areaOfCircle(double radius) {
		return Math.PI * radius * radius;
	}

	public static double force(double mass, double accel) {
		return mass * accel;
	}
	
	public static void welcome() {
		System.out.println("Welcome to Surrey!");
	}	

}
