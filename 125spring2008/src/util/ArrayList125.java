package util;

/*
 * ArrayList125 extends Java's ArrayList class by adding a constructor and an add
 * method that make it easy to add multiple values at once, e.g.
 * 
 *   ArrayList125<Integer> nums = new ArrayList125<Integer>(1, 2, 3, 4, 5);
 * 
 * creates a new ArrayList125 object with the given initial values. You can also
 * call add to append multiple values, e.g.
 * 
 *   nums.add(-1, -2, 3, 4);
 * 
 */

import java.util.ArrayList;
import java.util.Collection;
import java.util.Random;

public class ArrayList125<T> extends ArrayList<T> {

	private static Random rnd = new Random();
	
    public ArrayList125() {
    }

    public ArrayList125(int initialCapacity) {
        super(initialCapacity);
    }

    public ArrayList125(Collection<T> c) {
        super(c);
    }

    public ArrayList125(T... args) {
        this(args.length);
        add(args);
    }
    
    public void add(T... args) {
        for (T x : args)
            add(x);
    }
    
    // remove and return a randomly chosen value
    public T removeRandom() {
    	int r = rnd.nextInt(size());
    	T x = remove(r);
    	return x;
    }
    
}
