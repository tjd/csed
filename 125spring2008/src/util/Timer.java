package util;

// A utility class for timing code. The timer starts when the object
// is created, and at any time you can ask how many milliseconds/seconds/etc.
// have elapsed. For example:
//
//    Timer timer = new Timer();  // the clock has started!
//
//    someCode();
//
//    System.out.printf("%.2f seconds elapsed", timer.getElapsedSeconds());

public class Timer {

    public final long startTime;
    
    public Timer() {
        startTime = System.currentTimeMillis();
    }
    
    public long getElapsedMilliseconds() {
        return System.currentTimeMillis() - startTime;
    }
    
    public double getElapsedSeconds() {
        return getElapsedMilliseconds() / 1000.0;
    }
    
    public double getElapsedMinutes() {
        return getElapsedSeconds() / 60.0;
    }
    
}
