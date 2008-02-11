package galaxy;

public class Alien extends Being {
    
    // create an alien with a random weight and intelligence
    public Alien(String name, String planet) {
        this(name, rnd.nextDouble() * 500, rnd.nextInt(26), planet);
    }
    
    public Alien(String name, double weight, int intelligence, String planet) {
        super(name, weight, intelligence, planet);
    }
    
}
