package range;

public class Range {

	private int begin;
	private int end;
	
	public Range() {
		begin = 0;
		end = 0;
	}
	
	public Range(int begin, int end) {
		this.begin = begin;
		this.end = end;
	}
	
	public int size() {
		return end - begin;
	}
	
	public boolean isEmpty() {
		return size() <= 0;
	}
	
	// mutators
	public void incBegin() {
		++begin;
	}
	
	public void decBegin() {
		--begin;
	}
	
	public void incEnd() {
		++end;
	}
	
	public void decEnd() {
		--end;
	}
	
	public boolean contains(int i) {
		return i >= begin && i < end;
	}
	
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
