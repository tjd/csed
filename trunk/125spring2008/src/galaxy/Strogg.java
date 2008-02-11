package galaxy;

public class Strogg extends Alien {

    // create a Strogg with a random weight and intelligence
    public Strogg(String name) {
        this(name, rnd.nextDouble() * 200, rnd.nextInt(26));
    }

    public Strogg(String name, double weight, int intelligence) {
        super(name, weight, intelligence, "Stroggos");
        this.addPossession("Strogg neurocyte");
    }

}
