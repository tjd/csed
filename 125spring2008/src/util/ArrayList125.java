package util;

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
        for (T x : args)
            add(x);
    }

}
