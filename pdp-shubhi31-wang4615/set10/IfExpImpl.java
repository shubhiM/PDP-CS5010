
import java.util.Map;

// Constructor template for IfExpImpl:
//   new IfExpImpl(testExp, theExp, elseExp)
// Interpretation:
//   an IfExp represents an 'if testExp then thenExp else elseExp fi'
// Example:
//   if a > b true; else false;
//   Exp testPart
//       = Asts.arithmeticExp(Asts.identifierExp ("a"),
//                           "GT",
//                            Asts.identifierExp ("b"));
//   Exp thenPart
//       = Asts.constantExp(Asts.expVal(true));
//   Exp elsePart
//       = Asts.constantExp(Asts.expVal(false));
//   ifExp ifex = Asts.ifExp (testPart,
//                       thenPart,
//                       elsePart);

public class IfExpImpl extends ExpBase implements IfExp {

    private Exp testExp; // if condition
    private Exp thenExp; // Expression when if condition is true
    private Exp elseExp; // Expression when if condition is false

    IfExpImpl(Exp testExp, Exp thenExp, Exp elseExp) {
        this.testExp = testExp;
        this.thenExp = thenExp;
        this.elseExp = elseExp;
    }

    // Returns: true because this object is an ifExp
    public boolean isIf() {
        return true;
    }


    // Precondition: this Exp is for an ifExp.
    // Returns: an ifExp like this one.
    public IfExp asIf() {
        return new IfExpImpl(this.testExp, this.thenExp, this.elseExp);
    }

    // Returns the test part of this if expression.
    public Exp testPart() {
     return this.testExp;
    }
    // Returns the then part of this if expression.
    public Exp thenPart() {
        return this.thenExp;
    }
    // Returns the else part of this if expression.
    public Exp elsePart() {
        return this.elseExp;
    }

    // Given: a environment mapping the identifiers to their values
    // Returns: if testExp is true then returns the value of the thenExp,
    //   otherwise returns the value of the elseExp.
    // Examples:
    //    HashMap<String, ExpVal> env = new HashMap<>();
    //      env.put("a", Asts.expVal(1));
    //      env.put("b", Asts.expVal(2));
    //  using ifex from above
    //  ifex.value(env) == Asts.expVal(false)

    public ExpVal value (Map<String,ExpVal> env) {

        ExpVal testExpVal = this.testExp.value(env);
        ExpVal resultVal;

        if (!testExpVal.isBoolean()) {
            throw new RuntimeException();
        }

        if(testExpVal.asBoolean()) {
            // test expression is true
            // evaluating then expression
            resultVal = this.thenExp.value(env);

        } else {
            resultVal = this.elseExp.value(env);
        }
        return resultVal;
    }
}

