package basics;

import java.util.Scanner;

public class ConsoleInput {

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        System.out.print("What is your name? ");
        String name = sc.next();
        System.out.print("How old are you? ");
        String age = sc.next();
        System.out.printf("\nHello %s! In 5 years you will be %s years old.", name,
                Integer.parseInt(age) + 5);
    }

}
