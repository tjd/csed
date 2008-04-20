package intro;

public class HelloWorld {

	public static void helloWorld() {
		System.out.println("Hello, world!");
	}

	public static void main(String[] args) {
		for (int i = 0; i < 300; ++i) {
			helloWorld();
		}
	}
}
