package galaxy;

import java.util.ArrayList;

public class TestBeing {

    public static void main(String[] args) {
        Being b = new Being("Bill Gates", 100, 20);
        b.print();
        
        Human h = new Human("George Bush", 100, 20);
        h.print();
        
        Martian m = new Martian("Marvin");
        m.print();
        
        Strogg s = new Strogg("Kane");
        s.print();
        
        ArrayList<Being> ship = new ArrayList<Being>();
        ship.add(b);
        ship.add(h);
        ship.add(m);
        ship.add(s);
       
        // give everyone on the ship a oxygen tank
        for(Being x : ship) {
            x.addPossession("oxygen tank");
        }
        
        // print them all out
        for(Being x : ship) {
            x.print();
        }
       
    }

}
