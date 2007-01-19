package util;

public class Util {
	
	// throws an error if assertions are not enabled in JVM
	public static void checkAssertions()
	{
		try {
			assert 1 == 2;
			throw new Error("Assertions not enabled!");
		} catch (AssertionError e) {
			// okay!
		}
	}
}
