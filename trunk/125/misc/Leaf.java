package misc;

import java.awt.Color;

public class Leaf {

	private Color color;
	
	public Leaf(Color c) {
		color = c;
	}
	
	public Leaf(int red, int green, int blue) {
		color = new Color(red, green, blue);
	}

	public Color getColor() {
		return color;
	}

	public void setColor(Color color) {
		this.color = color;
	}

	public void print() {
		System.out.printf("leaf(%s)\n", color);
	}
	
	public static void main(String[] args) {
		Leaf a = new Leaf(Color.RED);
		Leaf b = new Leaf(45, 254, 82);
		a.print();
		b.print();
	}
	
}
