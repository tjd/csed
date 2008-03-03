package simplelist;

public class ListFunctions {

	public static void main(String[] args) {
		List lst = List.empty.push(4).push(3).push(2).push(1);
		System.out.printf("lst = \"%s\"\n", toString(lst));
		System.out.printf("length = %s\n", length(lst));
		System.out.printf("sum(%s) = %s\n", toString(lst), sum(lst));
		System.out.printf("product(%s) = %s\n", toString(lst), product(lst));

		System.out.printf("contains(%s, %s) = %s\n", toString(lst), 2,
				contains(lst, 2));
		System.out.printf("contains(%s, %s) = %s\n", toString(lst), 22,
				contains(lst, 22));

		System.out.printf("last(%s) = %s\n", toString(lst), last(lst));

		System.out.printf("dropLast(%s) = %s\n", toString(lst),
				toString(dropLast(lst)));

		System.out.printf("append(%s, -1) = %s\n", toString(lst),
				toString(append(lst, -1)));

		System.out.printf("reverse1(%s) = %s\n", toString(lst),
				toString(reverse1(lst)));
		
		List lst2 = List.empty.push(5).push(4).push(3).push(2).push(1);
		
		System.out.printf("reverse1(%s) = %s\n", toString(lst2),
				toString(reverse1(lst2)));
		
		System.out.printf("reverse2(%s) = %s\n", toString(lst),
				toString(reverse2(lst)));
		System.out.printf("reverse2(%s) = %s\n", toString(lst2),
				toString(reverse2(lst2)));
		
		System.out.printf("reverse3(%s) = %s\n", toString(lst),
				toString(reverse3(lst)));
		System.out.printf("reverse3(%s) = %s\n", toString(lst2),
				toString(reverse3(lst2)));
	}

	
	public static List reverse3(List lst) {
		return reverse3(lst, List.empty);
	}
	
	public static List reverse3(List lst, List rev) {
		if (lst.isEmpty()) {
			return rev;
		} else {
			return reverse3(lst.rest(), rev.push(lst.first()));
		}
	}
	
	public static List reverse2(List lst) {
//		System.out.printf("debug: reverse2(%s)\n", toString(lst));
		if (lst.isEmpty()) {
			return lst;
		} else if (length(lst) == 1) {
			return lst;
		} else {
			int f = lst.first();
			int l = last(lst);
			List mid = dropLast(lst.rest());
			List temp1 = reverse2(mid);
			List temp2 = temp1.push(l);
			return append(temp2, f);
		}
	}
	
	public static List reverse1(List lst) {
		if (lst.isEmpty()) {
			return List.empty;
		} else {
			return append(reverse1(lst.rest()), lst.first());
		}
	}

	public static List append(List lst, int x) {
		if (lst.isEmpty()) {
			return List.empty.push(x);
		} else {
			return append(lst.rest(), x).push(lst.first());
		}
	}

	public static List dropLast(List lst) {
//		System.out.printf("debug: dropLast(%s)\n", toString(lst));
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
