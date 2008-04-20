package oop.character;

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
		
		// set default values
		this.gold = 25;
		this.xp = 10;
		this.hp = 12;
		this.stateOfMind = State.normal;
		this.strength = new Attribute("strength", 0, 20);
		this.agility = new Attribute("agility", 0, 20);
	}

	public double getAgility() {
		return this.agility.getValue();
	}

	public void setAgility(double agility) {
		this.agility.setValue(agility);
	}

	public int getLevel() {
		return this.getXp() / 100;
	}

	public State getStateOfMind() {
		return this.stateOfMind;
	}

	public void setStateOfMind(State stateOfMind) {
		this.stateOfMind = stateOfMind;
	}

	public double getStrength() {
		return this.strength.getValue();
	}

	public void setStrength(double strength) {
		this.strength.setValue(strength);
	}

	public String getName() {
		return this.name.getName();
	}

	public int getGold() {
		return this.gold;
	}

	public void setGold(int gold) {
		this.gold = gold;
	}

	public int getHp() {
		return this.hp;
	}

	public void setHp(int hp) {
		this.hp = hp;
	}

	public int getXp() {
		return this.xp;
	}

	public void setXp(int xp) {
		this.xp = xp;
	}

	public String toString() {
		String result = "";
		result += "Character name: " + name + "\n";
		result += "Gold: " + gold + "\n";
		result += "Experience points: " + xp + "\n";
		result += "Hit points: " + hp + "\n";
		result += "State of mind: " + stateOfMind + "\n";
		result += strength + "\n";
		result += agility + "\n";
		return result;
	}

}

enum State {
	normal, sleepy, dizzy, confused, afraid
}
