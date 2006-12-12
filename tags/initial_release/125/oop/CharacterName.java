package oop;

public class CharacterName {

	private String rawName;

	private String name;

	public CharacterName(String s) {
		this.rawName = s;
		this.name = s.trim().replace(' ', '_');
	}

	public String getName() {
		return name;
	}

	public String getRawName() {
		return rawName;
	}
		
}
