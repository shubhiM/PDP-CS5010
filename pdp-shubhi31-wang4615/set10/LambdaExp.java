// A LambdaExp is an object of any class that implements LambdaExp.
//
// Interpretation: A LambdaExp represents a lambda expression.

import java.util.List;

interface LambdaExp extends Exp {

    // Returns: the formal parameters of this lambda expression.
    List<String> formals();

    // Returns: the body of this lambda expression.
    Exp body();
}
