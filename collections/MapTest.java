package collections;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;

public class MapTest {
  
    public static void main(String[] args) {
        HashMap<String, String> map = new HashMap<String, String>();
        map.put("apple", "A red fruit.");
        map.put("animal", "A vegetable that eats and sleeps.");
        map.put("brook", "Babbling water.");
        map.put("cherry", "A red fruit.");
        map.put("wombat", "Nobody knows what this is.");
        map.put("zwieback", "Crunchy toast.");

        System.out.println("" + map);
        ArrayList<String> terms = new ArrayList<String>(map.keySet());
        Collections.sort(terms);
        
        for(String word: terms) {
        	String def = map.get(word);
        	System.out.printf("%s - %s\n", word, def);
        }
    }
}
