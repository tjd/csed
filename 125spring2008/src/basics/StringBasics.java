package basics;

public class StringBasics {

    public static void main(String[] args) {
        String name = "Goodtime Charlie";
        int len = name.length();
        System.out.printf("The string \"%s\" is %s characters long.\n", name,
                len);

        String gossip = name + " is quite the elbow-lifter.";
        System.out.printf("%s\n", gossip);

        String sign1 = "You need " + 3 + " coupons to ride the Smirbler.";
        String sign2 = "Number of coupons needed: " + 3;
        String sign3 = "Number of coupons needed: " + 1 + 2;

        System.out.printf("%s\n%s\n%s\n", sign1, sign2, sign3);
    }

}
