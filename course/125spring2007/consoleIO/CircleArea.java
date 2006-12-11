package consoleIO;

import java.util.Scanner;

public class CircleArea {

	public static double circleArea(double radius) {
		return Math.PI * radius * radius;
	}

	public static void main(String[] args) {
		Scanner sc = new Scanner(System.in);
		
		System.out.printf("Enter the circle's radius: ");
		double radius = sc.nextDouble();
		
		if (radius < 0) {
			System.out.printf("Oops, the radius must be positive.\n");
		} else {
			double area = circleArea(radius);
			System.out.printf("The area of a circle with radius %s is %s.", radius, area);
		}
	}

}
