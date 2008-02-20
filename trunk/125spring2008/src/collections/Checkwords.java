package collections;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashSet;
import java.util.Scanner;

public class Checkwords {

    public static void main(String[] args) throws FileNotFoundException {
        //
        // create the dictionary
        //
        
        //ospd.txt is a list of English words, one word per line; it's available on-line from
        // http://csed.googlecode.com/svn/trunk/125spring2008/ospd.txt
        final String DICT_FILENAME = "/home/toby/svn/tjd-personal/125/code/ospd.txt";
        
        System.out.printf("Creating dictionary ...");
        HashSet<String> words = new HashSet<String>();
        Scanner dict = new Scanner(new File(DICT_FILENAME));
        while (dict.hasNextLine()) {
            words.add(dict.nextLine().trim());
        }
        System.out.printf(" %s words read\n", words.size());
        
        //
        // main loop
        //
        Scanner sc = new Scanner(System.in);
        System.out.print("checkword> ");
        String input = sc.nextLine().trim().toLowerCase();
        while (!input.equals("done")) {
            if (words.contains(input)) {
                System.out.printf("\"%s\" is in the dictionary!\n\n", input);
            } else {
                System.out.printf("unknown word\n\n");
            }
            System.out.print("checkword> ");
            input = sc.nextLine().trim().toLowerCase();
        }
    }

}
