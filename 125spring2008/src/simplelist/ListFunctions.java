package simplelist;

public class ListFunctions {

	public static void main(String[] args) {
		List lst = List.empty.push(4).push(3).push(2).push(1);
		System.out.printf("lst = \"%s\"\n", toString(lst));
		System.out.printf("length = %s\n", length(lst));
		System.out.printf("sum(%s) = %s\n", toString(lst), sum(lst));
		System.out.printf("product(%s) = %s\n", toString(lst), product(lst));
		
		System.out.printf("contains(%s, %s) = %s\n", toString(lst), 2, contains(lst, 2));
		System.out.printf("contains(%s, %s) = %s\n", toString(lst), 22, contains(lst, 22));
		
		System.out.printf("last(%s) = %s\n", toString(lst), last(lst));
		
		System.out.printf("dropLast(%s) = %s\n", toString(lst), toString(dropLast(lst)));
	}
	
	public static List dropLast(List lst) {
		if (length(lst) == 1) {
			return List.empty;
		} else {
			return dropLast(lst.rest()).push(lst.first());
		}
	}
	
	public static int last(List lst) {
		if (length(lst) == 1) {
			return lst.first();
		} else {
			return last(lst.rest());
		}
	}
	
	public static boolean contains(List lst, int x) {
		if (lst.isEmpty()) {
			return false;
		} else {
			if (lst.first() == x) {
				return true;
			} else {
				return contains(lst.rest(), x);
			}
		}
	}

	public static int length(List lst) {
		if (lst.isEmpty()) {
			return 0;
		} else {
			return 1 + length(lst.rest());
		}
	}

	public static String toString(List lst) {
		return ("(" + _toString(lst) + ")").trim();
	}

	private static String _toString(List lst) {
		if (lst.isEmpty()) {
			return "";
		} else {
			return lst.first() + " " + _toString(lst.rest());
		}
	}

	public static int sum(List lst) {
		if (lst.isEmpty()) {
			return 0;
		} else {
			return lst.first() + sum(lst.rest());
		}
	}

	public static int product(List lst) {
		if (lst.isEmpty()) {
			return 1;
		} else {
			return lst.first() * product(lst.rest());
		}
	}



}
