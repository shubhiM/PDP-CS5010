import java.util.*;

// TestRunner is a single test class that runs test cases for all individual
// expressions, values and also for programs.


public class TestRunner {

   public static void main(String[] args) {
      
      // *******************************************************
      // Tests for Programs.java
      Programs pgm = new Programs();
      Set<String> result0 = pgm.undefined("bad.ps11");
      assert result0.containsAll(Arrays.asList("x", "z")) && result0.size() == 2;

      result0 = pgm.undefined("church.ps11");
      assert result0.isEmpty();

      result0 = pgm.undefined("sieve.ps11");
      assert result0.isEmpty();

      // *******************************************************
      // Example lambdas for testing
      // example 1
      // lambda (a) {a + 2}

      List<String> formals1 = Asts.list("a");

      LambdaExp lambda1 = Asts.lambdaExp(
              formals1,
              Asts.arithmeticExp(
                      Asts.identifierExp("a"),
                      "PLUS",
                      Asts.constantExp(
                              Asts.expVal(2))));

      // example 2
      // lambda (a, b) { a + b }
      List<String> formals2 = Asts.list("a", "b");

      LambdaExp lambda2 = Asts.lambdaExp(
              formals2,
              Asts.arithmeticExp(
                      Asts.identifierExp("a"),
                      "PLUS",
                      Asts.identifierExp("b")));

      // example 3
      // lambda (a, b) {if a > b true; else false; }
      Exp testPart
          = Asts.arithmeticExp(Asts.identifierExp ("a"),
                              "GT",
                               Asts.identifierExp ("b"));
      Exp thenPart
          = Asts.constantExp(Asts.expVal(true));
      Exp elsePart
          = Asts.constantExp(Asts.expVal(false));

      LambdaExp lambda3 = Asts.lambdaExp(
              formals2,
              Asts.ifExp (testPart,
                          thenPart,
                          elsePart));
      // example 4
      // lambda (a) { plus2 (plus2 (a) ) }
      LambdaExp lambda4 = Asts.lambdaExp(
              formals1,
              Asts.callExp(Asts.identifierExp ("plus2"),
                           Asts.list(
                             Asts.callExp(
                                Asts.identifierExp ("plus2"),
                                Asts.list (Asts.identifierExp ("a"))))));  
      
      // example 5
      // lambda(f, a) { f(a) }
      List<String> formals5 = Asts.list("f", "a");
      LambdaExp lambda5 = Asts.lambdaExp(
                formals5,
                Asts.callExp(Asts.identifierExp ("f"),
                             Asts.list(
                                Asts.identifierExp("a"))));
      // example 6
      // lambda(a) { a + (lambda(a)(a + 2))(10) }
      LambdaExp lambda6 = Asts.lambdaExp(
                formals1,
                Asts.arithmeticExp(
                      Asts.identifierExp("a"),
                      "PLUS",
                      Asts.callExp(lambda1,
                                   Asts.list(
                                      Asts.constantExp(Asts.expVal(10))))));


      // ******************************************************
      // Programs Testing

      // plus2(a) a + 2
      Def def1 = Asts.def("plus2", lambda1);
      ExpVal result = Programs.run(Asts.list(def1), 
                                   Asts.list(Asts.expVal(5)));
      assert result.asInteger() == 7;
      
      // plus(a, b) a + b
      Def def2 = Asts.def("plus", lambda2);
      result = Programs.run(Asts.list(def2),
                                   Asts.list(Asts.expVal(10), Asts.expVal(-5)));
      assert result.asInteger() == 5;



      // compare(a, b)
      //   if a > b
      //      then 
      //        True
      //      else 
      //        False
      //   fi
      Def def3 = Asts.def("compare", lambda3);
      result = Programs.run(Asts.list(def3),
                            Asts.list(Asts.expVal(3), Asts.expVal(2)));
      assert result.asBoolean();
      result = Programs.run(Asts.list(def3),
                            Asts.list(Asts.expVal(2), Asts.expVal(2)));
      assert result.asBoolean() == false;

      // plus4(a) {plus2(plus2(a))}
      // plus2(b) {b + 2}
      Def def4 = Asts.def("plus4", lambda4);
      result = Programs.run(Asts.list(def4, def1),
                      Asts.list(Asts.expVal(3)));
      assert result.asInteger() == 7;

      // fact (n) 
      //   if n = 0 
      //     then 1 
      //     else n * fact (n - 1) 
      //   fi
      Exp exp5
          = Asts.arithmeticExp (Asts.identifierExp ("n"),
                                "MINUS",
                                Asts.constantExp (Asts.expVal (1)));
      Exp call5
          = Asts.callExp (Asts.identifierExp ("fact"),
                          Asts.list (exp5));
      Exp testPart2
          = Asts.arithmeticExp (Asts.identifierExp ("n"),
                                "EQ",
                                Asts.constantExp (Asts.expVal (0)));
      Exp thenPart2
          = Asts.constantExp (Asts.expVal (1));
      Exp elsePart2
          = Asts.arithmeticExp (Asts.identifierExp ("n"),
                                "TIMES",
                                call5);
      LambdaExp factorial 
          = Asts.lambdaExp (Asts.list ("n"),
                                      Asts.ifExp (testPart2,
                                                  thenPart2,
                                                  elsePart2));

      Def def5
          = Asts.def ("fact",
                      factorial);
      result = Programs.run (Asts.list (def5),
                                    Asts.list (Asts.expVal (9)));
      assert result.asInteger() == 362880;

      // exec(f, a) { f(a) }
      HashMap<String, ExpVal> env6 = new HashMap<>();

      Def def6 = Asts.def("exec", lambda5);
      // example 1 
      // exec(plus2(), 9)
      result = Programs.run (Asts.list (def6),
                             Asts.list (Asts.expVal(lambda1, env6), 
                                        Asts.expVal (9)));
      assert result.asInteger() == 11;
      // example 2 
      // exec(fact(), 9)
      // result = Programs.run (Asts.list (def6, def5),
      //                        Asts.list (Asts.expVal(factorial, env6), 
      //                                   Asts.expVal (9)));
      // assert result.asInteger() == 362880; 

   
      // plus12(a) { a + (lambda(a)(a + 2))(10) }
      Def def7 = Asts.def("plus12", lambda6);
      result = Programs.run (Asts.list (def7),
                             Asts.list (
                                  Asts.expVal (9)));
      assert result.asInteger() == 21; 


      // ********** class Implementations for testing
      // ********************************************

      // Test ExpValImpl
      ExpVal expLong = Asts.expVal((long) 2017);
      assert expLong.isInteger() == true;
      assert expLong.asInteger() == 2017;

      ExpVal expBool = Asts.expVal(false);
      assert expBool.isBoolean() == true;
      assert expBool.asBoolean() == false;

      // ********************************************
      // Test ConstantExpImpl
      ConstantExp consLong = Asts.constantExp(expLong);
      assert consLong.isConstant() == true;
      ExpVal consLongV = consLong.value();
      assert consLongV.isInteger() == true;
      assert consLongV.asInteger() == 2017;


      // ********************************************
      // Test IdentifierExpImpl
      String expString = "test";
      IdentifierExp iExp = Asts.identifierExp(expString);
      assert iExp.isIdentifier() == true;
      assert iExp.name() == expString;


      // ********************************************
      // Test ArithmeticExpImpl

      // case 1
      // aExp1 represents 1 - 2
      ArithmeticExp aExp1 = Asts.arithmeticExp(
              Asts.constantExp(Asts.expVal(1)),
              "MINUS",
              Asts.constantExp(Asts.expVal(2)));

      // environment for aExp1
      HashMap<String, ExpVal> env = new HashMap<>();

      // result for aExp1
      ExpVal res = Asts.expVal(-1);

      // General tests for arithmetic type of expressions
      assert aExp1.isArithmetic() == true;
      ExpVal actual = aExp1.value(env);

      assert isEqual(actual, res) == true;


      // case 2
      // Exp: 1 - 2 + 3 * 4

      ArithmeticExp aExp2 =  Asts.arithmeticExp(
              Asts.constantExp(Asts.expVal(3)),
              "TIMES",
              Asts.constantExp(Asts.expVal(4)));

      ArithmeticExp aExp3 = Asts.arithmeticExp(
               aExp1,
               "PLUS",
               aExp2);

      ExpVal aExp3Result = aExp3.value(env);
      ExpVal rExp3 = Asts.constantExp(Asts.expVal(11)).value(env);

      assert isEqual(aExp3Result, rExp3) == true;

      // case3
      // Exp: (a + (b + (3 * (4 * 5))))
      // a: 1
      // b: 2

      // environment for aExp1
      HashMap<String, ExpVal> env2 = new HashMap<>();
      env2.put("a", Asts.expVal(1));
      env2.put("b", Asts.expVal(2));

      ArithmeticExp aexp4 = Asts.arithmeticExp(
              Asts.identifierExp("a"),
              "PLUS",
              Asts.arithmeticExp(
                      Asts.identifierExp("b"),
                      "PLUS",
                      Asts.arithmeticExp(
                              Asts.constantExp(Asts.expVal(3)),
                              "TIMES",
                              Asts.arithmeticExp(
                                      Asts.constantExp(Asts.expVal(4)),
                                      "TIMES",
                                      Asts.constantExp(Asts.expVal(5))
                             )
                      )
              )
      );

      ExpVal aexp4Result = aexp4.value(env2);
      ExpVal aexp4Expected = Asts.expVal(63);

      assert isEqual(aexp4Expected, aexp4Result) == true;

      // ******************************************************
      // Testing IfExpImpl

      // case 1
      //  if 2 > 1 then 2 else 1 fi

      IfExp ifExp1 = Asts.ifExp(
              Asts.arithmeticExp(
                      Asts.constantExp(Asts.expVal(2)),
                      "GT",
                      Asts.constantExp(Asts.expVal(1))
              ),
              Asts.constantExp(Asts.expVal(2)),
              Asts.constantExp(Asts.expVal(1))
      );

      ExpVal actualIfExp1 = ifExp1.value(env);
      ExpVal expectedIfExp1 = Asts.expVal(2);

      assert isEqual(actualIfExp1, expectedIfExp1) == true;


      // case 2
      //  if (a + b) > 4 then a else b fi
      // a: 1
      // b: 2
      // if (1 + 2) > 4 then 1 else 2

      IfExp ifExp2 = Asts.ifExp(
              Asts.arithmeticExp(
                      Asts.arithmeticExp(
                              Asts.identifierExp("a"),
                              "PLUS",
                              Asts.identifierExp("b")
                      ),
                      "GT",
                      Asts.constantExp(Asts.expVal(4))
              ),
              Asts.identifierExp("a"),
              Asts.identifierExp("b")
      );

      ExpVal actualifExp2 = ifExp2.value(env2);
      ExpVal expectedifExp2 = Asts.expVal(2);

      assert isEqual(actualifExp2, expectedifExp2) == true;
      
      // ******************************************************
      // Test CallExpImpl

      // example env
      Map<String,ExpVal> env0 = new HashMap<String, ExpVal>();
      Map<String,ExpVal> env1 = new HashMap<String, ExpVal>();

      // case 1:
      // plus2 (x) = x + 2
      env1.put("plus2", Asts.expVal(lambda1, env0));

      List<Exp> arg = new LinkedList<Exp>();
      arg.add(Asts.constantExp(Asts.expVal(10)));
      
      Exp call1 = Asts.callExp(
            Asts.identifierExp("plus2"),
            arg);
      ExpVal res1 = call1.value(env1);
      assert res1.asInteger() == 12;



      // *****************************************************

       // Testing for global and local environment scope
       // Testing for multiple definitions


       // Program:
       // function callA(a) { a * 4}
       // function callB(b) {b * 5 }
       // function scopeTesting (a, b) {
       //       callA(a) * callB(b) + a + b }


       List<String> formals10 = new LinkedList<>();

       List<String> formals1a = new LinkedList<>();
       List<String> formals1b = new LinkedList<>();

       formals1a.add("a");
       formals1b.add("b");

       formals10.add("a");
       formals10.add("b");

       ArithmeticExp exp11 = Asts.arithmeticExp(
               Asts.identifierExp("a"),
               "TIMES",
               Asts.constantExp(Asts.expVal(4)));

       ArithmeticExp exp12 = Asts.arithmeticExp(
               Asts.identifierExp("b"),
               "TIMES",
               Asts.constantExp(Asts.expVal(5)));

       CallExp call10 = Asts.callExp(
               Asts.identifierExp("callA"), Asts.list(Asts.identifierExp
                       ("a")));
       CallExp call11 = Asts.callExp(
               Asts.identifierExp("callB"), Asts.list(Asts.identifierExp
                       ("b")));


       LambdaExp lambda10 = Asts.lambdaExp(
               formals10,
               Asts.arithmeticExp(
                       Asts.arithmeticExp(call10, "TIMES", call11),
                               "PLUS",
                               Asts.arithmeticExp(
                                       Asts.identifierExp("a"),
                                       "PLUS",
                                       Asts.identifierExp("b")
                               )
                       )
       );

       LambdaExp lambda11 = Asts.lambdaExp(formals1a, exp11);
       LambdaExp lambda12 = Asts.lambdaExp(formals1b, exp12);

       Def def10 = Asts.def("scopeTesting", lambda10);

       Def def11 = Asts.def("callA",  lambda11);
       Def def12 = Asts.def("callB", lambda12);

       ExpVal result10 = Programs.run(Asts.list(def10, def11, def12), Asts.list
               (Asts.expVal
               (1), Asts
               .expVal
               (2)));

       assert result10.asInteger() == 43;
  }
  
  // helper function to compare two ExpVals.
  // Given: two ExpVals val1 and val2.
  // Returns: true iff the two expVal both represents integer or 
  //   boolean and have the same value;
  public static boolean isEqual(ExpVal val1, ExpVal val2) {
      if(val1.isBoolean()) {
          return val1.asBoolean() == val2.asBoolean();
      } else if(val1.isInteger()) {
          return val1.asInteger() == val2.asInteger();
      } else {
          throw new RuntimeException();
      }
  }
}





