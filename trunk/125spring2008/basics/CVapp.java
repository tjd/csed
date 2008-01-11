package basics;

import java.util.Scanner;

/*

 The SFU CV system requires that users type in a semester in CYYS
 form, where

 C is the century, 0 for the 1900s, 1 for the 2000s
 YY is the last two digits of the year
 S is the semester, where spring is 1, summer is 4, and fall is 7

 For example:

 Fall Semester 2007 = 1077
 Spring Semester 1995 = 0951
 Summer Semester 2000 = 1004

 Needless to say this is a rather lazy interface --- users
 shouldn't be forced to encode data for the computer, especially
 when it is so easy to do it automatically!

 */

public class CVapp {

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        System.out.printf("What year? ");
        String year = sc.next();
        System.out.printf("What semester (spring, summer, or winter)? ");
        String sem = sc.next();
        String cyys = cyysCode(sem, year);
        System.out.printf("\nThe CYYS code for %s %s is %s.", year, sem, cyys);
    }

    public static String cyysCode(String sem, String year) {
        int sc = semesterCode(sem);
        int y = Integer.parseInt(year);
        // get the century
        int c = centuryCode(y);
        // get the last two chars of year
        int len = year.length();
        String yy = year.substring(len - 2);

        return "" + c + yy + sc;
    }

    public static int centuryCode(int year) {
        if (year < 2000) {
            return 0;
        } else {
            return 1;
        }
    }

    // return the CYYS code for the given semester
    public static int semesterCode(String sem) {
        if (sem.equals("spring")) {
            return 1;
        } else if (sem.equals("summer")) {
            return 4;
        } else if (sem.equals("winter")) {
            return 7;
        } else {
            return -1; // error!
        }
    }

}
