package oop.diary;


/* 
 
Slow but simple sorting algorithm. Don't use this if you need to quickly 
sort a lot of data!

 s.compareTo(t) returns 
 
   - a negative integer is s comes before t;
   - 0 if s equals t
   - a positive integer if s comes after t

Look at the JavaDoc definition of String: the String class implements
the Comparable<String> interface. An interface is a collection of method headers,
and for a class to say that it implements a particular interface means
the class promises to provide implemented methods with exactly those headers.

Here is the definition of the Comparable<String> interface:

   public interface Comparable<String> {
      public int compareTo(String o);
   }
   
The idea here is that any class that implements the Comparable<Strnig> interface
will provide the compareTo method for comparing objects. We use an
interface because different objects will have different criteria for how
they want to compare themselves.

For example, in the Email class we could compare emails just by username,
while ignoring the extension. Or perhaps we had some reason for sorting
the emails first by extension, and then by username. We can implement
the Email compareTo method whatever way we need.

Lets make our own interface. Suppose you want to keep track of basic information
about certain objects, such as who owns the object, when it was created,
what type of object it is, and so on. Such information would be useful in a file
system, or any a document management program where you need to keep track of such
information.

Here's the interface:

public interface Information {
  public String getOwner();
  public java.util.Date getDateCreated();
  public String getType();
  public void setType(String type);
}

Now any class you wish can implement this interface. It is then up to the class 
to provide implementations of each of these methods. This is really elegant
idea because it completely seperates the interface from implementation.

As an analogy, consider electrical outlets like you would find in your house.
In North America, at least, outles have standard geometric shape, and a
standard electrical output. You can plug any device into any outlet, now matter
what the device is, and now matter how the outlet draws its electricity. It
doesn't matter if the outlet is running on solar power, hydroelectric power, 
geothermal power, or a hamster in a wheel. All that matters is that it delivers 
on its promise of delivering
a certain amount of electricity.

Electrical outlets also give a nice example of extending a class. Original outlets 
only allowed two-pronged plugs. But over time, a third ground plug was added
(http://en.wikipedia.org/wiki/Electrical_outlet). In Java, we could denote the situation
like this:
 
interface Outlet {
   public Electricity leftProng();
   public Electricity rightProng();
}

interface GroundedOutlet extends Outlet {
   public Ground groundProng();
}

The beauty of this approach is that if you have a two-pronged plug, you
can plug it into either outlet. That's because the GroundedOutlet has not
modifed the basic Outlet; it simply extends, adding extra functionality. You can
think of it as the grounded outlet remaining backwards compatible with
the regular outlet.

Of course, a three-pronged plug won't work in a two-pronged outlet.
*/

import java.util.Arrays;


public class StringSort {

	public static void bubblesort(String[] arr) {
		boolean done = false;
		while (!done) {
			done = true;
			for (int i = 1; i < arr.length; ++i) {
				if (arr[i - 1].compareTo(arr[i]) > 0) {
					String temp = arr[i - 1];
					arr[i - 1] = arr[i];
					arr[i] = temp;
					done = false;
				}
			}
		}
	}

	public static <T extends Comparable<T>> void bubblesort(T[] arr) {
		boolean done = false;
		while (!done) {
			done = true;
			for (int i = 1; i < arr.length; ++i) {
				if (arr[i - 1].compareTo(arr[i]) > 0) {
					T temp = arr[i - 1];
					arr[i - 1] = arr[i];
					arr[i] = temp;
					done = false;
				}
			}
		}
	}

	public static void test1() {
		String[] arr = { "cat", "bat", "house", "up", "yum", "toast", "tug" };
		bubblesort(arr);
		System.out.printf("%s", Arrays.toString(arr));
	}

	public static void test2() {
		Email[] arr = { new Email("merve@gmail.com"),
				new Email("merve@hotmail.com"), new Email("mj1@sfu.ca"),
				new Email("carl@sfu.ca"), new Email("pleg34@misty.net"),
				new Email("vivaldi@sirrus.com") };
		bubblesort(arr);
		System.out.printf("%s", Arrays.toString(arr));
	}

	public static void main(String[] args) {
		// test1();
		test2();
	}

}
