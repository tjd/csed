package million;

/* 
 * How long does it take to guess a number out of a million possibilities?
 * 
 */

import java.util.Random;

import util.Timer;

public class Million {

    public static void main(String[] args) {
        final int LIMIT = 1000000;
        Random rnd = new Random();
        int winningNumber = rnd.nextInt(LIMIT);

        int count = 1;
        Timer timer = new Timer();
        while (rnd.nextInt(LIMIT) != winningNumber) {
            count++;
        }

        System.out.printf("It took %.2fs and %s guesses to randomly choose %s",
                timer.getElapsedSeconds(), count, winningNumber);

    }

}
