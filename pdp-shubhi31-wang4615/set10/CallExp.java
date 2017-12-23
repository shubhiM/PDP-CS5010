// A CallExp is an object of any class that implements CallExp.
//
// Interpretation: A CallExp represents a function call.

import java.util.List;

interface CallExp extends Exp {

    // Returns the expression for the function part of the call.
    Exp operator();

    // Returns the list of argument expressions.
    List<Exp> arguments();
}
