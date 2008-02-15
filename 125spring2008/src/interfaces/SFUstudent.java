package interfaces;

public class SFUstudent extends Student {
	private int age;
	private int[] id;

	public String getStudentID() {
		String result = "";
		for (Integer x : id) {
			result += x;
		}
		return result;
	}
	
	public int getAge() {
		return age;
	}
	
	public void setFavouriteColor(String c) {
		
	}
}
