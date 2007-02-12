package myvector;

public class MyVector {

	//
	// private variables and methods
	//
	private int[] arr;

	private int end;

	// doubles the length of the underlying array
	private void doubleSize() {
		int[] arr2 = new int[2 * arr.length];
		for (int i = 0; i < arr.length; ++i) {
			arr2[i] = arr[i];
		}
		arr = arr2;
	}

	//
	// Constructors
	//

	// create an empty vector with capacity of 10
	public MyVector() {
		arr = new int[10];
		end = 0;
	}

	//
	// getters
	//

	// returns the number of items in this vector
	public int size() {
		return end;
	}

	// returns true iff this MyVector has no elements
	public boolean isEmpty() {
		return size() == 0;
	}

	// returns the size of the underlying array
	// the larger the capacity, the more calls to push that can
	// can be made without needing to call doubleSize()
	public int capacity() {
		return arr.length;
	}

	// returns value at location i
	public int get(int i) {
		assert i < size();
		return arr[i];
	}

	// returns first element
	public int first() {
		return get(0);
	}

	// returns last element
	public int last() {
		return get(size() - 1);
	}

	// returns a new MyVector that contains everything but the first
	// element of this MyVector
	// Note: Calling rest() makes a copy of the underlying vector, which,
	// if called frequently, could be very inefficient.
	public MyVector rest() {
		assert size() > 0;
		MyVector result = new MyVector();
		for (int i = 1; i < size(); ++i) {
			result.push(get(i));
		}
		return result;
	}

	//
	// mutators
	//

	// sets location i to be value
	public void set(int i, int value) {
		assert i < size();
		arr[i] = value;

	}

	// append x to the end of the vector; automatically increases
	// capacity if necessary
	public void push(int x) {
		if (size() == capacity()) {
			doubleSize();
		}
		arr[end] = x;
		++end;
	}

	// returns a String representation of this object; note that his
	// overrides Object.toString, so you can write code like this:
	//
	// System.out.println("myvec = " + myvec);
	//
	@Override
	public String toString() {
		if (size() == 0) {
			return "[]";
		} else if (size() == 1) {
			return "[" + arr[0] + "]";
		} else {
			// size() >= 2
			String result = "[" + arr[0];
			for (int i = 1; i < end; ++i) {
				result += ", " + arr[i];
			}
			return result + "]";
		}
	}
}

/*
 * 
 * Questions
 * 
 * 1. In the doubleSize() method, a new array twice the length of arr is
 * created. After the elements of arr are copied into it, "arr = arr2" makes arr
 * point to the just created array. What happened to the array arr originally
 * pointed to?
 * 
 * 2. Add a constructor that sets the initial capacity of the underlying array
 * to be a given value, e.g. so you can write code like this:
 * 
 * MyVector vec = new MyVector(100); for(int i = 1; i <= 100; ++i) { vec.add(i); }
 * 
 * 3. Using the constructor you just created in the previous question, make a
 * simple change to the rest() function that speeds it up.
 * 
 * 4. A copy constructor is a constructor that takes in an object of the class's
 * type, and a returns a new object that is a copy of the passed-in object. Add
 * a copy constructor to MyVector.
 * 
 * 5. What is the value of including the first() and last() methods? Why not
 * just have the user use the get method?
 * 
 * 6. What is the purpose of the ensure method?
 * 
 * 7. A number of methods called "ensure(i < size())" to make sure that the
 * value of i refers to a location inside the array. What happens if i is less
 * than zero?
 * 
 * 8. MyVector's default constructor (i.e. the constructor that has no
 * parameters) sets the initial length of the underlying array to be 10. There
 * is no deep reason for this value: 10 seems like a reasonable number, and
 * other implementations of dynamic arrays use similar values. Better would be
 * to choose a value that is based on actual evidence of what makes a good
 * initial value. So, propose an experiment that you could do to better
 * determine a good intial capacity for MyVector.
 * 
 * 9. Implement a boolean method called "same" that takes in another MyVector,
 * and returns true if it is equal to the current one, and false otherwise. Two
 * MyVector's are considered equal if they have the same values in the same
 * order (the capacities may be different).
 * 
 * 10. Write a mutator method that reverses the order of the elements of a
 * MyVector. Include 5 or more good test cases to help show that it works
 * correctly.
 * 
 * 11. Add a method called toArray() that returns an array of ints containing
 * the values of the MyVector. The length of the returned array should be
 * size().
 * 
 * 12. Add a constructor that takes an array of ints as the input parameter.
 * 
 * 
 */