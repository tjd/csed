package misc;

public class Bool {

	public static boolean trueMessage() {
		System.out.println("trueMessage called");
		return true;
	}
	
	public static boolean falseMessage() {
		System.out.println("falseMessage called");
		return false;
	}
	
	public static void main(String[] args) {
		boolean a = true;
		boolean b = false;
		
		boolean x = (!falseMessage() || trueMessage())  
		             && (trueMessage() || !trueMessage());
	}
	
}
