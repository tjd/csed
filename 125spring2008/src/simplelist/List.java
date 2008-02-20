package simplelist;

/*
 * This List class stores ints, and supports the following five basic 
 * operations:
 * 
 *   - List.empty: Returns an empty list.
 *   - first(): Returns the first value of the list.   
 *   - rest(): Returns a new list that is the same as the original list,
 *     but with the first item removed.
 *   - push(x): Return a new list that is the same as the original list,
 *     but a new item has been added to the front.
 *   - isEmpty(): Returns true if the list is empty, and false otherwise.
 *   
 * This is a minimalist set of list operations; in contrast, the Java 
 * LinkedList class has over 50 methods.
 * 
 * This style of list is the fundamental data structure in many so-called
 * functional languages, such as LISP or Haskell. It is especially good for
 * recursive programming.
 * 
 * Theses lists are also immutable: you cannot modify a list once you've 
 * created it. All you can do is create other lists. A nice consequence of 
 * this is that lists, and parts of lists, can be freely shared among each 
 * other without fear of a change to one list corrupting another. For 
 * instance, there is only a single empty list that is shared by all lists.
 * 
 */

public class List {

    public final static List empty = new List() {
        public boolean isEmpty() {
            return true;
        }
    };

    private int value;

    private List next;

    private List() {
        this(0, null);
    }

    private List(int x, List next) {
        this.value = x;
        this.next = next;
    }

    public int first() {
        return value;
    }

    public List rest() {
        return next;
    }

    public List push(int x) {
        return new List(x, this);
    }

    public boolean isEmpty() {
        return false;
    }

}
