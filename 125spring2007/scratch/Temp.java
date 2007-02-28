package scratch;

public class Temp {
	public static void swap(int x, int y) {
//		System.out.printf("x = %s, y = %s\n", x, y);
		int temp = x;
		x = y;
		y = temp;
//		System.out.printf("x = %s, y = %s\n", x, y);
	}

	public static void main(String[] args) {
		int a = 4;
		int b = 6;
		System.out.printf("a = %s, b = %s\n", a, b);
		swap(a, b);
		System.out.printf("a = %s, b = %s\n", a, b);
	}

}
