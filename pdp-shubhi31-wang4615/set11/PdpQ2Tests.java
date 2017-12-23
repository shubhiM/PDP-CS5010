import java.util.HashSet;
import java.util.Set;

public class PdpQ2Tests {

    public static void main(String[] args) {
        Set<String> none = new HashSet<String>();
        Set<String> bad1 = new HashSet<String>();
        Set<String> bad2 = new HashSet<String>();
        Set<String> bad3 = new HashSet<String>();
        Set<String> bad4 = new HashSet<String>();

        bad1.add ("m");
        bad1.add ("n");
        bad1.add ("add4");

        bad2.add ("x");

        bad3.add ("y");
        bad3.add ("x");
        bad3.add ("M");
        bad3.add ("N");

        bad4.add ("z");
        
        PdpTestSuite tests = new PdpTestSuite(60);
        
        String[] good = {
                "ack", "bst", "church", "fact", "fact2", "fib",
                "insertionsort", "mergesort", "naive", "sieve", "tail"
        };
        for(String p : good) {
            tests.addTestCase(p, () -> undefined(p), none);
        }
        tests.addTestCase("bad1", () -> undefined("bad1"), bad1);
        tests.addTestCase("bad2", () -> undefined("bad2"), bad2);
        tests.addTestCase("bad3", () -> undefined("bad3"), bad3);
        tests.addTestCase("bad4", () -> undefined("bad4"), bad4);
        
        tests.runTests();
    }

    private static Set<String> undefined (String name) {
        name = "PS11testPrograms/" + name + ".ps11";
        return Programs.undefined (name);
    }

}
