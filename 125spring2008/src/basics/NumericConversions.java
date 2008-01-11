package basics;

public class NumericConversions {

    public static void main(String[] args) {
        double rate = 0.22;
        String rateAsAstring = "" + rate;
        System.out.printf("rate = %s, rateAsAstring = %s\n", rate, rateAsAstring);
        
        String cost = "3.43";
        double costAsAsdouble = Double.parseDouble(cost);
        System.out.printf("cost = %s, costAsAsdouble = %s\n", cost, costAsAsdouble);
    }

}
