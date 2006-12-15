package oop;

public class Character {

	private CharacterName name;
	private int gold;
	private int xp;
	private int hp;
	private State stateOfMind;
	private Attribute strength;
	private Attribute agility;
	
	public Character(String name) {
		this.name = new CharacterName(name);
		this.gold = 25;
		this.xp = 10;
		this.hp = 12;
		this.stateOfMind = State.normal;
		this.strength = new Attribute("strength", 0, 20);
		this.agility = new Attribute("agility", 0, 20);
	}
	
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

	public double getAgility() {
		return agility.getValue();
	}

	public void setAgility(double agility) {
		this.agility.setValue(agility);
	}

	public int getLevel() {
		return getXp() / 100;
	}

	public State getStateOfMind() {
		return stateOfMind;
	}

	public void setStateOfMind(State stateOfMind) {
		this.stateOfMind = stateOfMind;
	}

	public double getStrength() {
		return strength.getValue();
	}

	public void setStrength(double strength) {
		this.strength.setValue(strength);
	}

	public String getName() {
		return name.getName();
	}

	public int getGold() {
		return gold;
	}

	public void setGold(int gold) {
		this.gold = gold;
	}

	public int getHp() {
		return hp;
	}

	public void setHp(int hp) {
		this.hp = hp;
	}

	public int getXp() {
		return xp;
	}

	public void setXp(int xp) {
		this.xp = xp;
	}

}


enum State {
	normal, sleepy, dizzy, confused, afraid
}
