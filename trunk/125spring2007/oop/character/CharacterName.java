package objects.character;

public class CharacterName {

	private String rawName;

	private String name;

	public CharacterName(String s) {
		this.rawName = s;
		this.name = s.trim().replace(' ', '_');
	}

	public String getName() {
		return this.name;
	}

	public String getRawName() {
		return this.rawName;
	}

	public String toString() {
		return getName();
	}
}
