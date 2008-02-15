package interfaces;

import java.util.ArrayList;

public class StudentTest {

	public static void main(String[] args) {
//		Student s = new Student();
		SFUstudent sfu = new SFUstudent();
		UBCstudent ubc = new UBCstudent();
		
		Criminal c = new Criminal();
		
		ArrayList<Student> lst = new ArrayList<Student>();
		lst.add(sfu);
		lst.add(ubc);
		
		ArrayList<Person> people = new ArrayList<Person>();
		people.add(sfu);
		people.add(ubc);
		people.add(c);
	}

}
