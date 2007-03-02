package scratch;

public class Wrapper {

	public static void main(String[] args) {
		Integer x = new Integer(51);
		System.out.println("x = " + x);

		int a = 4;
		Integer b = a;
		int c = x + a;
		
		Integer d = 1 + 2 + 3 + new Integer(4);
		System.out.println("a = " + a);
		System.out.println("b = " + b);
		System.out.println("c = " + c);
		System.out.println("d = " + d);
	}

}
