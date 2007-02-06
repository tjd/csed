package stack;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Scanner;
import java.util.Stack;

public class Calc {

	private CalcStack stack;

	private Dict dict;

	private UserDefinedStackFun funBeingDefined;

	class CalcStack extends Stack<String> {
		public int pop_int() {
			return Integer.parseInt(pop());
		}
	}
	
	class Dict extends HashMap<String, StackFun> {
		public void put(StackFun sf) {
			this.put(sf.getKey(), sf);
		}
	}

	public void run_interpreter() {
		Scanner sc = new Scanner(System.in);
		while (true) {
			System.out.print("--> ");
			String line = sc.nextLine();
			String[] tokens = line.split(" ");
			process_one_line(tokens);
		}
	}

	public void process_one_line(String[] tokens) {
		for (String tok : tokens) {
			if (dict.containsKey(tok)) {
				// System.out.println("in dict: " + tok);
				if (funBeingDefined == null || tok.equals(";")) {
					dict.get(tok).call_fun(stack);
				} else {
					funBeingDefined.add(tok);
				}
			} else {
				try {
					if (funBeingDefined == null) {
						Integer.parseInt(tok);
						stack.push(tok);
					} else {
						funBeingDefined.add(tok);
					}
				} catch (NumberFormatException e) {
					System.out.printf("Error: '%s' is undefined\n", tok);
				}
			}
		}
	}

	public Calc() {
		stack = new CalcStack();
		dict = new Dict();
		funBeingDefined = null;
		
		dict.put(new Add());
		dict.put(new Subtract());
		dict.put(new Multiply());
		dict.put(new Print());
		dict.put(new Bye());
		dict.put(new StartDef());
		dict.put(new TerminateDef());
		dict.put(new ListFun());
		dict.put(new Dup());
		dict.put(new Drop());
		dict.put(new Swap());
		dict.put(new ShowStack());
	}

	public abstract class StackFun {
		public abstract void call_fun(CalcStack s);

		public abstract String getKey();
		
		public String toString() {
			return getKey() + " <system defined>";
		}
	}

	public abstract class UserDefinedStackFun extends StackFun {
		public abstract void add(String tok);
	}

	public class Add extends StackFun {
		public String getKey() {
			return "+";
		}

		public void call_fun(CalcStack s) {
			int a = s.pop_int();
			int b = s.pop_int();
			s.push("" + (a + b));
		}
	}

	public class Subtract extends StackFun {
		public String getKey() {
			return "-";
		}

		public void call_fun(CalcStack s) {
			int a = s.pop_int();
			int b = s.pop_int();
			s.push("" + (b - a));
		}
	}

	public class Multiply extends StackFun {
		public String getKey() {
			return "*";
		}

		public void call_fun(CalcStack s) {
			int a = s.pop_int();
			int b = s.pop_int();
			s.push("" + (a * b));
		}
	}

	public class Print extends StackFun {
		public String getKey() {
			return ".";
		}

		public void call_fun(CalcStack s) {
			System.out.println(s.peek());
		}
	}

	public class Bye extends StackFun {
		public String getKey() {
			return "bye";
		}

		public void call_fun(CalcStack s) {
			System.out.println("Bye bye!");
			System.exit(0);
		}
	}

	public class Dup extends StackFun {
		public String getKey() {
			return "dup";
		}

		public void call_fun(CalcStack s) {
			s.push(s.peek());
		}
	}

	public class Drop extends StackFun {
		public String getKey() {
			return "drop";
		}

		public void call_fun(CalcStack s) {
			s.pop();
		}
	}

	public class Swap extends StackFun {
		public String getKey() {
			return "swap";
		}

		public void call_fun(CalcStack s) {
			int n = s.size();
			String temp = s.get(n - 2);
			s.set(n - 2, s.get(n - 1));
			s.set(n - 1, temp);
		}
	}

	public class ListFun extends StackFun {
		public String getKey() {
			return "list";
		}

		public void call_fun(CalcStack s) {
			for (String key : dict.keySet()) {
				System.out.printf("%s: %s\n", key, dict.get(key).toString());
			}
		}
	}

	public class ShowStack extends StackFun {
		public String getKey() {
			return "show";
		}

		public void call_fun(CalcStack s) {
			System.out.println(stack);
		}
	}


	public class StartDef extends StackFun {
		public String getKey() {
			return ":";
		}

		public void call_fun(CalcStack s) {
			// System.out.println("StartDef");
			funBeingDefined = new UserDefinedFunction();
		}
	}

	public class TerminateDef extends StackFun {
		public String getKey() {
			return ";";
		}

		public void call_fun(CalcStack s) {
			// System.out.println("TerminateDef");
			dict.put(funBeingDefined.getKey(), funBeingDefined);
			funBeingDefined = null;
		}
	}

	public class UserDefinedFunction extends UserDefinedStackFun {
		private ArrayList<String> arr = new ArrayList<String>();

		public void call_fun(CalcStack s) {
			String[] tokens = new String[arr.size() - 1];
			for (int i = 1; i < arr.size(); ++i) {
				tokens[i - 1] = arr.get(i);
			}
			process_one_line(tokens);
		}

		public void add(String s) {
			arr.add(s);
		}

		public String getKey() {
			return arr.get(0);
		}

		public String toString() {
			return arr.toString();
		}
	}

}
