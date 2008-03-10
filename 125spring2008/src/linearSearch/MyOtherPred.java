package linearSearch;

class MyOtherPred implements Predicate {
	public boolean f(int x) {
		return (x >= 0) && (x <= 100);
	}
}
