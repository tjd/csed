package oop.character;

public class CharacterTest {

	public static void main(String[] args) {
		Character c = new Character("Thresh");
		System.out.println(c);
		c.setStateOfMind(State.sleepy);
		c.setHp(c.getHp() + 5);
		System.out.println("\n" + c);
	}

}
