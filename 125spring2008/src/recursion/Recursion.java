package recursion;

public class Recursion {

	public static void main(String[] args) {
		rep(10);
	}

	// private static int count ;

	private static void rep(int n) {
		if (n > 0) {
			System.out.println("Yeah baby!");
			rep(n - 1);
		}
//		if (n <= 0) {
//			// stop
//		} else {
//			System.out.println("Yeah baby!");
//			rep(n - 1);
//		}
	}

}
