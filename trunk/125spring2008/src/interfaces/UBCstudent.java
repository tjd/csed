package interfaces;

public class UBCstudent extends Student {
	private long idNum;
	private int age;
	private String color;

	public String getStudentID() {
		return "" + idNum;
	}
	
	public int getAge() {
		return age;
	}
	
	public void setFavouriteColor(String c) {
		color = c;
	}
}
