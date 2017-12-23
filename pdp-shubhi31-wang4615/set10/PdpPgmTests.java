import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class PdpPgmTests {
    
    public static Exp constant(boolean b) {
        return Asts.constantExp(Asts.expVal(b));
    }
    
    public static Exp constant(long n) {
        return Asts.constantExp(Asts.expVal(n));
    }
    
    public static Exp id(String s) {
        return Asts.identifierExp(s);
    }
    
    @SafeVarargs
    public static <T> List<T> list(T... vs) {
        return Arrays.asList(vs);
    }

    public static void main(String[] args) {
        PdpTestSuite tests = new PdpTestSuite(60);
        
        //gcd (i, j)
        //  if i = j
        //    then i
        //    else if i > j
        //      then gcd (i - j, j)
        //      else gcd (i, j - i)
        //    fi
        //  fi;
        
        tests.addTestCase("gcd #1", () -> {
            Exp elseif = Asts.ifExp(Asts.arithmeticExp(id("i"), "GT", id("j")),
                    Asts.callExp(id("gcd"), list(
                            Asts.arithmeticExp(id("i"), "MINUS", id("j")),
                            id("j"))),
                    Asts.callExp(id("gcd"), list(
                            id("i"),
                            Asts.arithmeticExp(id("j"), "MINUS", id("i")))));
            Exp gcd = Asts.lambdaExp(
                    list("i", "j"),
                    Asts.ifExp(Asts.arithmeticExp(id("i"), "EQ", id("j")),
                            id("i"),
                            elseif));
            return Programs.run(
                    list(Asts.def("gcd", gcd)),
                    list(Asts.expVal(91), Asts.expVal(144)))
                    .asInteger();
        }, 1L);

        // basic tests

        addTest(tests, "f (x) 34", 97, 34);
        addTest(tests, "f (n) n + 1", 5, 6);
        addTest(tests, "f (n) g(n-3); g (x) x - 1", 5, 1);
        addTest(tests, "fact (n) if n = 0 then 1 else n * fact (n - 1)",
                 5, 120);
        addTest(tests, "fib (n) if n < 2 then n else fib (n-1) + fib(n-2)",
                 20, 6765);

        // Euler totient function

        addTest(tests, "phi (n) (count ((\u03bb (k) coprime (k,n)))) (1,n);" +
                 "count (pred) (\u03bb (i, j) loop (pred, i, j, 0));" +
                 "loop (p, i, j, sum)" +
                 "  if i > j" +
                 "    then sum" +
                 "    else loop (p," +
                 "               i+1," +
                 "               j," +
                 "               sum + (if p(i) then 1 else 0));" +
                 "coprime (i, j) gcd (i, j) = 1;" +
                 "gcd (i, j)" +
                 "  if i = j" +
                 "    then i" +
                 "  else if i > j" +
                 "    then gcd (i - j, j)" +
                 "  else gcd (i, j - i)",
                 28, 12);

        // Mutating an environment will be a common mistake.

        addTest(tests, "f (n) g (n);" +
                 "g (f) if f < 2 then i (f) else h (f-1, f-2);" +
                 "i (g) g;" +
                 "h (i, g) f(i) + f(g)",
                 10, 55);

        // Inner product

        addTest(tests, readPgm ("vectors.ps11"), 10, 285);
        addTest(tests, readPgm ("vectors.ps11"), 200, 2646700);

        // Church numerals

        addTest(tests, readPgm ("church.ps11"), 4, 24);
        addTest(tests, readPgm ("church.ps11"), 6, 720);

        // Sieve of Eratosthenes

        addTest(tests, readPgm ("sieve.ps11"), 2, 100, 25);
        addTest(tests, readPgm ("sieve.ps11"), 100, 200, 21);

        // Binary search trees

        addTest(tests, readPgm ("bst.ps11"), 5, 3, 1, 9);
        addTest(tests, readPgm ("bst.ps11"), 50, 17, 10, 289);

        tests.runTests();
    }


    static void addTest (PdpTestSuite tests, String pgm0, long... inputsAndExpected) {
        final int numInputs = inputsAndExpected.length - 1;
        long expected = inputsAndExpected[numInputs];
        int maxLen = 40;
        String pgm1 = pgm0.replace("\n", "");
        String testName =  pgm1.length() > maxLen ?
                pgm1.substring(0, maxLen - 3) + "..." :
                pgm1;
        tests.addTestCase(testName,
                () -> {
                    List<ExpVal> inputs = new ArrayList<ExpVal>();
                    for (int i = 0; i < numInputs; i++) {
                        inputs.add(Asts.expVal(inputsAndExpected[i]));
                    }
                    List<Def> defs = parsePgm(pgm0);
                    return Programs.run(defs, inputs).asInteger();
                },
                expected);
    }

    static List<Def> parsePgm (String pgm0) {
        return Scanner.parsePgm (pgm0);
    }

    static String readPgm (String filename) {
        return Scanner.readPgm (filename);
    }
}
