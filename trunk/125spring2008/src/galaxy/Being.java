package galaxy;

import java.util.ArrayList;
import java.util.Random;

public class Being {

	// create one random number for all Beings; there's no reason to give
	// each Being its own
	public final static Random rnd = new Random();

	// Every Being has three main attributes: a name, a weight, and intelligence
	// rating
	private String name;

	private double weightInKG;

	private int intelligence;

	private String planetOfOrigin;

	private ArrayList<String> possessions;

	public Being(String name, double startingWeight, int startingIntelligence) {
		this(name, startingWeight, startingIntelligence, "Unknown");
	}

	public Being(String name, double startingWeight, int startingIntelligence,
			String planet) {
		this.name = name;
		this.weightInKG = startingWeight;
		this.intelligence = startingIntelligence;
		this.planetOfOrigin = planet;
		this.possessions = new ArrayList<String>();
	}

	public int getIntelligence() {
		return intelligence;
	}

	public void setIntelligence(int intelligence) {
		this.intelligence = intelligence;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public double getWeightInKG() {
		return weightInKG;
	}

	public void setWeightInKG(double weightInKG) {
		this.weightInKG = weightInKG;
	}

	public String getPlanetOfOrigin() {
		return this.planetOfOrigin;
	}

	// returns a copy of the list of possessions; does not return the actual
	// list
	public ArrayList<String> getListOfPossessions() {
		ArrayList<String> copy = new ArrayList<String>(this.possessions.size());
		for(String s : this.possessions) {
			copy.add(s);
		}
		return copy;
	}

	public void addPossession(String thing) {
		this.possessions.add(thing);
	}

	public String toString() {
		String result = "\n";
		result += "        Name: " + this.getName() + "\n";
		result += "      Origin: " + this.getPlanetOfOrigin() + "\n";
		result += String
				.format("      Weight: %.1f kg\n", this.getWeightInKG());
		result += "Intelligence: " + this.getIntelligence() + "\n";
		result += " Possessions: " + this.getListOfPossessions();
		return result;
	}

	public void print() {
		System.out.println(this);
	}

}
