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
 * appens the four given values to the end of nums.
 * 
 */

import java.util.ArrayList;
import java.util.Collection;

public class ArrayList125<T> extends ArrayList<T> {

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

}