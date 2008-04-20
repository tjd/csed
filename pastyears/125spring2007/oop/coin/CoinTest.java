package oop.coin;

import java.util.ArrayList;

/*
 * Suppose you want to simulate this coin flipping-game:
 * 
 *     n people each flip a coin at the same time.
 *     Those who get tails are out.
 *     Those who get heads flip again.
 *     How many times do you expect this to be repeated?
 * 
 * In object-oriented programming, it is natural to write a special
 * class to represent a coin. In this case, we have two different
 * implementations: Coin1 and Coin2. Both classes do the same thing,
 * but they have very different internal implementations. Coin1 uses
 * a boolean value to store what side is up, while Coin2 uses an
 * enumerated type.
 * 
 * Both implementations are reasonable, and neither has any major
 * advantage over the other.
 * 
 * Java provides the following interesting feature: you can write the
 * code for playing the coin-flipping game in a way that doesn't
 * care whether you use Coin1 or Coin2 to represent the coins.
 * 
 * The essential insight is that even though Coin1 and Coin2 differ in
 * their inernals, they have the same methods. In other words, the 
 * "interface" that Coin1 and Coin2 provide to programmers is
 * identical. Any code that uses a Coin1 object will only call these
 * methods:
 * 
 *    - the Coin1 constructor
 *    - public void flip()
 *    - public boolean isHeads()
 *    - public boolean isTails()
 * 
 * Coin2 has exactly these same methods, and so we could substitute
 * a Coin2 object instead.
 * 
 * We could just use cut-and-paste in a text editor to replace every
 * Coin1 with Coin2. It's a bit tedious, but it works.
 * 
 * Something you definitely cannot do with cut-and-paste is to allow
 * both Coin1 and Coin2 objects to appear in the same ArrayList. This
 * could conceivably happen if you were getting coin objects from other
 * programs, and you don't know ahead of time if you are getting Coin1's
 * or Coin2's. There's no way to put both Coin1 and Coin2 into all the
 * appropriate places in the code.
 * 
 * The solution to these problems is simple and elegant: Java interfaces.
 * Here is the Coin interface:
 * 
 *   public interface Coin {
 * 	   public void flip(); 
 *     public boolean isHeads();
 *     public boolean isTails();
 *   }
 * 
 * A Java interface is essentially a list of method headers, with no
 * bodies. An interface does not provide any code, just headers.
 * 
 * If you look carefully at the flipping program code, you'll see that
 * the only methods of coin objects that are called are among these
 * three.
 * 
 * To use the Coin interface, we modify the Coin1 and Coin2 classes
 * to implement it like this:
 * 
 *    class Coin1 implements Coin { ... }
 * 
 *    class Coin2 implements Coin { ... }
 *    
 * The "implements Coin" statement is all we need to add in this case, 
 * because Coin1 and Coin2 already implement the appropriately named 
 * methods. This statement is a promise: the Coin1 and Coin2 class
 * promise to implement all the methods listed in the Coin interface.
 * The Java compiler helps keep this promise; if you don't have all
 * the required methods, then the program will not compile.
 * 
 * Note that it is find if Coin1 or Coin2 happen to have more methods
 * than those in the Coin interface. Extra methods can always be
 * ignored. 
 * 
 * With the Coin interface set up, the next thing is to replace all
 * occurrences of Coin1 in the flipping code with Coin. For example,
 * the flippers ArrayList is defined like this now: 
 * 
 *    ArrayList<Coin> flippers = new ArrayList<Coin>();
 *    
 * This says that flippers is an ArrayList of objects that implement
 * the Coin interface. At the moment only Coin1 and Coin2 objects
 * implement Coin, but this code will later be able to hold *any*
 * object that implements Coin. 
 * 
 * The one little glitch is that contructors don't work out very
 * nicely. Java constructors are always named after the class
 * they construct, and so there is no way to get rid of the
 * name Coin1 in this code:
 * 
 *    for(int i = 0; i < n; ++i) {
 *	     flippers.add(new Coin1());
 *    }
 * 
 * But you can change this to Coin2 with no problems, or you
 * could even fill it with Coin1 and Coin2 objects like this:
 * 
 * 		for (int i = 0; i < n / 2; ++i) {
 *			flippers.add(new Coin1());
 *		}
 *
 *		for (int i = n / 2; i < n; ++i) {
 *			flippers.add(new Coin2());
 *		}
 *
 * This is a big deal: the code using the Coin interface
 * doesn't care about the exact details of the underlying
 * object that implements it. This lets us re-use chunks
 * of code with any object that implements Coin.
 * 
 * 
 */

public class CoinTest {

	public static void main(String[] args) {
		flipOut(10000);
	}

	public static void flipOut(int n) {
		// initialize the list of flippers
		ArrayList<Coin> flippers = new ArrayList<Coin>();
		for (int i = 0; i < n / 2; ++i) {
			flippers.add(new Coin1());
		}

		for (int i = n / 2; i < n; ++i) {
			flippers.add(new Coin2());
		}

		// perform the experiment
		int count = 0;
		while (!flippers.isEmpty()) {
			System.out.printf("%s: %s remain\n", count, flippers.size());
			flipAll(flippers);
			flippers = removeTails(flippers);
			++count;
		}
	}

	public static void flipAll(ArrayList<Coin> flippers) {
		for (Coin c : flippers) {
			c.flip();
		}
	}

	public static ArrayList<Coin> removeTails(ArrayList<Coin> flippers) {
		ArrayList<Coin> result = new ArrayList<Coin>();
		for (Coin c : flippers) {
			if (c.isHeads()) {
				result.add(c);
			}
		}
		return result;
	}

}
