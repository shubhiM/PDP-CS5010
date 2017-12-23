import java.util.List;
import java.util.Map;


// Constructor template for LambdaExpImpl:
//   new LambdaExpImpl(formals, body)
// Interpretation:
//   formals is a list of Strings that represents
//   name of bound variables
//   body is a expression
// Example: lambda (a) {a + 2}
// List<String> formals = Asts.list("a");
// LambdaExp lambda = Asts.lambdaExp(
//         formals,
//         Asts.arithmeticExp(
//                 Asts.identifierExp("a"),
//                 "PLUS",
//                 Asts.constantExp(
//                         Asts.expVal(2))));

public class LambdaExpImpl extends ExpBase implements LambdaExp {

    private List<String> formals; // represents bound variables
    private Exp body; // represents expression to be executed

    public LambdaExpImpl( List<String> formals, Exp body) {
        this.formals = formals;
        this.body = body;
    }

    // Returns: true because this object is an LambdaExp
    public boolean isLambda() {
        return true;
    }


    // Precondition: this Exp is for an LambdaExp.
    // Returns: an LambdaExp like this one.
    public LambdaExp asLambda() {
        return new LambdaExpImpl(this.formals, this.body);
    }

    // Returns the formal parameters of this lambda expression.
    public List<String> formals() {
        return this.formals;
    }

    // Returns the body of this lambda expression.
    public Exp body() {
        return this.body;
    }

    // Given: an environment
    // Returns: a FunVal (as an ExpVal) with the given environment
    //   and this lambda as the code.
    public ExpVal value (Map<String,ExpVal> env) {
        return Asts.expVal(this.asLambda() ,env);
    }

}
