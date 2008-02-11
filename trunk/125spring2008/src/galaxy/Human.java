package galaxy;

public class Human extends Being {

    private String favouriteColor;
    
    // create a human with a random weight and intelligence
    public Human(String name) {
        this(name, rnd.nextDouble() * 200, rnd.nextInt(26));
    }
    
    public Human(String name, double weight, int intelligence) {
        this(name, weight, intelligence, "Unknown");        
    }
    
    public Human(String name, double weight, int intelligence, String color) {
        super(name, weight, intelligence, "Earth");
        this.favouriteColor = "Unknown";            
    }

    public String getFavouriteColor() {
        return favouriteColor;
    }

    public void setFavouriteColor(String favouriteColor) {
        this.favouriteColor = favouriteColor;
    }

    
    public String toString() {
        String result = super.toString();
        result += "\n       Color: " + this.getFavouriteColor();
        return result;
    }
}
