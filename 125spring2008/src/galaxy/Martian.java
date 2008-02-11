package galaxy;

public class Martian extends Alien {
    
    // create a Martian with a random weight and intelligence
    public Martian(String name) {
        this(name, rnd.nextDouble() * 200, rnd.nextInt(26));
    }
    
    public Martian(String name, double weight, int intelligence) {
        super(name, weight, intelligence, "Mars");
    }
 
    public double weightInMarsUnits() {
        return this.getWeightInKG() * 0.66;
    }
    
}
