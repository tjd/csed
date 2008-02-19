package collections;

import java.util.HashMap;
import java.util.Scanner;

public class Glossary {

    public static void main(String[] args) {
        // HashMaps store (key, value) pairs
        HashMap<String, String> glossary = new HashMap<String, String>();

        // the following definitions are taken from
        // http://www.toonopedia.com/glossary.htm

        glossary.put("Bigfoot",
                     "A style of cartoon art characterized by bold, expressive lines,"
                        + " a lack of distracting detail, and humorous exaggeration of bodily"
                        + " features such as noses and feet.");
        
        glossary.put("In-between", 
                     "In animation, an in-between drawing shows a character's" 
                        + " transitional pose between two extremes.");
        
        glossary.put("Indicia", 
                     "A paragraph of small type, found in most magazines (including" 
                        + " comic books), indicating the magazine's title, date," 
                        + " publisher, and other relevant information.");
        
        glossary.put("Word balloon",
                     "The area in which a comics character's speech appears, usually with a" 
                        + " \"tail\" pointing to the one speaking. Usually, balloons are both" 
                        + "positioned and drawn by the letterer. Sometimes called \"word bubble\""
                        + "speech bubble\" or \"dialog bubble\", especially in Europe.");
        
        System.out.printf("Keys: %s\n", glossary.keySet());
        
        // let use enter terms and get definitions
        Scanner sc = new Scanner(System.in);
        System.out.print("Enter term> ");
        String input = sc.nextLine().trim();
        while (!input.toLowerCase().equals("done")) {
            String result = glossary.get(input);
            if (result == null) {
                System.out.println("No such term.\n");
            } else {
                System.out.printf("result = \"%s\"\n\n", result);
            }
            System.out.print("Enter term> ");
            input = sc.nextLine().trim();
        }
    }

}
