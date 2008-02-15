package interfaces;

public abstract class Student implements Person {

	public abstract String getStudentID();
	
	public final static String PROVINCE = "BC";
	
	public String getName() {
		return "Bill";
	}
	
	public abstract int getAge();
	
	public abstract void setFavouriteColor(String c); 
	

}
