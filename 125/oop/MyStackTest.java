package oop;

public class MyStackTest {

	/**
	 * @param args
	 */
public static void main(String[] args) {
		MyStack<Integer> istack = new MyStack<Integer>();

		istack.println();
		istack.push(2);
		istack.println();
		istack.push(5);
		istack.println();
		istack.push(1);
		istack.println();
		int p = istack.pop();
		System.out.printf("popped p = %s", p);
		istack.println();
		p = istack.pop();
		System.out.printf("popped p = %s", p);
		istack.println();
		p = istack.pop();
		System.out.printf("popped p = %s", p);
		istack.println();
		p = istack.pop();
	}
}
