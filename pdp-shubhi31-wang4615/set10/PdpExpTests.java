import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class PdpExpTests {
    
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
    
    public static Exp parseExp (String exp) {
        return Scanner.parseExp(exp);
    }
    
    public static Map<String, ExpVal> testEnv() {
        Map<String, ExpVal> envTemp = new HashMap<>();
        envTemp.put("inc", Asts.expVal(
                Asts.lambdaExp(list("x"), Asts.arithmeticExp(id("x"), "PLUS", constant(1))),
                Collections.emptyMap()));
        envTemp.put("x", Asts.expVal(7));
        envTemp.put("y", Asts.expVal(13));
        return Collections.unmodifiableMap(envTemp);
    }

    public static void main(String[] args) {
        PdpTestSuite tests = new PdpTestSuite(10);

        /* 
         * Verify that expressions correctly identify themselves
         */

        tests.addTestCase("true is a constant",
                () -> constant(true).isConstant(),
                true);
        tests.addTestCase("false is a constant",
                () -> constant(false).isConstant(),
                true);
        tests.addTestCase("42 is a constant",
                () -> constant(42).isConstant(),
                true);
        tests.addTestCase("0 is a constant",
                () -> constant(0).isConstant(),
                true);
        // (concrete syntax does not support negative integers)
        tests.addTestCase("lambda is not a constant",
                () -> Asts.lambdaExp(list("x"), id("x")).isConstant(),
                false);
        
        tests.addTestCase("sparticus is an identifier",
                () -> id("sparticus").isIdentifier(),
                true);
        tests.addTestCase("true is not an identifier",
                () -> constant(true).isIdentifier(),
                false);
        
        tests.addTestCase("lambda identifies as such",
                () -> Asts.lambdaExp(list("x"), id("x")).isLambda(),
                true);
        tests.addTestCase("lambda can have many formals",
                () -> Asts.lambdaExp(
                        list("x", "y", "z", "w", "v", "u", "t", "s"),
                        id("x")).isLambda(),
                true);
        tests.addTestCase("PLUS is not a lambda",
                () -> Asts.arithmeticExp(
                        constant(2), "PLUS", constant(2)).isLambda(),
                false);
        
        tests.addTestCase("PLUS is arithmetic",
                () -> Asts.arithmeticExp(
                        constant(2), "PLUS", constant(2)).isArithmetic(),
                true);
        tests.addTestCase("lambda is not arithmetic",
                () -> Asts.lambdaExp(list("x"), id("x")).isArithmetic(),
                false);
        
        tests.addTestCase("call identifies as such",
                () -> Asts.callExp(
                        id("f"),
                        list(id("x"))).isCall(),
                true);
        tests.addTestCase("call can have lambda as operator",
                () -> Asts.callExp(
                        Asts.lambdaExp(list("x"), id("x")),
                        list(id("x"))).isCall(),
                true);
        tests.addTestCase("call can have call as operator",
                () -> Asts.callExp(
                        Asts.callExp(
                                Asts.lambdaExp(list("x"), id("x")),
                                Arrays.asList(Asts.identifierExp("x"))),
                        list(id("x"))).isCall(),
                true);
        tests.addTestCase("PLUS is not a call",
                () -> Asts.arithmeticExp(
                        constant(2), "PLUS", constant(2)).isCall(),
                false);
        
        tests.addTestCase("if identifies as such",
                () -> Asts.ifExp(constant(true), id("x"), id("y")).isIf(),
                true);
        tests.addTestCase("lambda is not an if",
                () -> Asts.lambdaExp(list("x"), id("x")).isIf(),
                false);

        /*
         * Evaluate simple expressions in an empty environment
         */
        
        final Map<String, ExpVal> empty = Collections.emptyMap();
        
        tests.addTestCase("false => false",
                () -> constant(false).value(empty).asBoolean(),
                false);
        tests.addTestCase("42 => 42",
                () -> constant(42).value(empty).asInteger(),
                42L);
        
        // (evaluating an identifier in an empty environment is unspecified)
        
        tests.addTestCase("(λ(x) x) => function with empty env",
                () -> Asts.lambdaExp(list("x"), id("x"))
                .value(empty).asFunction().environment().containsKey("x"),
                false);
        
        tests.addTestCase("(λ(x) x)(5010) => 5010",
                () -> Asts.callExp(
                        Asts.lambdaExp(list("x"), id("x")),
                        list(constant(5010)))
                .value(empty).asInteger(),
                5010L);
        
        tests.addTestCase("(λ(x) x)((λ(x) x))(5010) => 5010",
                () -> Asts.callExp(
                        Asts.callExp(
                                Asts.lambdaExp(list("x"), id("x")),
                                list(Asts.lambdaExp(list("x"), id("x")))),
                        list(constant(5010)))
                .value(empty).asInteger(),
                5010L);
        
        tests.addTestCase("3 < 2 => false",
                () -> Asts.arithmeticExp(constant(3), "LT", constant(2))
                .value(empty).asBoolean(),
                false);
        tests.addTestCase("3 = 2 => false",
                () -> Asts.arithmeticExp(constant(3), "EQ", constant(2))
                .value(empty).asBoolean(),
                false);
        tests.addTestCase("3 > 2 => true",
                () -> Asts.arithmeticExp(constant(3), "GT", constant(2))
                .value(empty).asBoolean(),
                true);
        tests.addTestCase("3 + 2 => 5",
                () -> Asts.arithmeticExp(constant(3), "PLUS", constant(2))
                .value(empty).asInteger(),
                5L);
        tests.addTestCase("3 - 2 => 1",
                () -> Asts.arithmeticExp(constant(3), "MINUS", constant(2))
                .value(empty).asInteger(),
                1L);
        tests.addTestCase("3 * 2 => 6",
                () -> Asts.arithmeticExp(constant(3), "TIMES", constant(2))
                .value(empty).asInteger(),
                6L);
        
        tests.addTestCase("if true does not evaluate else branch",
                () -> Asts.ifExp(constant(true), constant(1), id("oops"))
                .value(empty).asInteger(),
                1L);
        tests.addTestCase("if false does not evaluate then branch",
                () -> Asts.ifExp(constant(false), id("oops"), constant(1))
                .value(empty).asInteger(),
                1L);

        /*
         * Evaluate expressions in a non-empty environment
         */
        
        tests.addTestCase("x * y",
                () -> Asts.arithmeticExp(id("x"), "TIMES", id("y"))
                .value(testEnv()).asInteger(),
                91L);
        
        tests.addTestCase("inc (x)",
                () -> Asts.callExp(id("inc"), list(id("x")))
                .value(testEnv()).asInteger(),
                8L);
        
        tests.addTestCase("inc (inc (x))",
                () -> Asts.callExp(id("inc"), list(Asts.callExp(id("inc"), list(id("x")))))
                .value(testEnv()).asInteger(),
                9L);
        
        tests.addTestCase("inc (y) - inc (x)",
                () -> Asts.arithmeticExp(
                        Asts.callExp(id("inc"), list(id("y"))),
                        "MINUS",
                        Asts.callExp(id("inc"), list(id("x"))))
                .value(testEnv()).asInteger(),
                6L);
                
        tests.addTestCase("(λ(f,x) f(f(x)))(inc,y)",
                () -> Asts.callExp(
                        Asts.lambdaExp(
                                list("f", "x"),
                                Asts.callExp(
                                        id("f"),
                                        list(Asts.callExp(id("f"), list(id("x")))))),
                        list(id("inc"), id("y")))
                .value(testEnv()).asInteger(),
                15L);
        
        /*
         * More expressions that use an environment
         */

        addTest(tests, "y < z", true);
        addTest(tests, "z < y", false);
        addTest(tests, "y < 5", false);
        addTest(tests, "y = z", false);
        addTest(tests, "z = y", false);
        addTest(tests, "y = 5", true);
        addTest(tests, "y > z", false);
        addTest(tests, "z > y", true);
        addTest(tests, "y > 5", false);

        addTest(tests, "y+z", 22);
        addTest(tests, "y-z", -12);
        addTest(tests, "y*z", 85);

        // these use set11 syntax, but whatever
        addTest(tests, "if y < z then 97 else 107", 97);
        addTest(tests, "if y = z then 97 else 107", 107);
        addTest(tests, "if x - y < x + z then 97 else 107", 97);
        addTest(tests, "if x - y > x + z then 97 else 107", 107);

        addTest(tests, "f (1000, 2)", 1034);
        addTest(tests, "y+z", 22);
        addTest(tests, "f (9999, 100)", 11699);
        addTest(tests, "(if y < z then f else 107) (100, 10)", 270);
        addTest(tests, "(\u03bb (x,y,z) f) (1,2,3) (10, 10)", 180);
        addTest(tests, "(\u03bb (f) (\u03bb (x) f (f, x)))" +
                 "((\u03bb (f, x)" +
                 "   if x < 2 then x else f (f, x - 1) + f (f, x - 2)))" +
                 "(20)",
                 6765);

        tests.runTests();
    }
    
    static Map<String, ExpVal> testEnv2() {
        LambdaExp lexp = parseExp ("(\u03bb (x,y) x+y*z)").asLambda();
        Map<String, ExpVal> env = new HashMap<>();
        env.put ("a", Asts.expVal (true));
        env.put ("b", Asts.expVal (false));
        env.put ("x", Asts.expVal (0));
        env.put ("y", Asts.expVal (5));
        env.put ("z", Asts.expVal (17));
        env.put ("f", Asts.expVal (lexp, env));
        return Collections.unmodifiableMap(env);
    }

    static void addTest (PdpTestSuite tests,
                         String exp0,
                         boolean expected) {
        Exp exp = parseExp (exp0);
        int maxLen = 40;
        String testName =  exp0.length() > maxLen ?
                exp0.substring(0, maxLen - 3) + "..." :
                exp0;
        tests.addTestCase(testName,
                () -> exp.value(testEnv2()).asBoolean(),
                expected);
    }

    static void addTest (PdpTestSuite tests,
                         String exp0,
                         long expected) {
        Exp exp = parseExp (exp0);
        int maxLen = 40;
        String testName =  exp0.length() > maxLen ?
                exp0.substring(0, maxLen - 3) + "..." :
                exp0;
        tests.addTestCase(testName,
                () -> exp.value(testEnv2()).asInteger(),
                expected);
    }

}
