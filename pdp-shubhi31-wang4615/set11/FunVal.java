// A FunVal is an object of any class that implements the FunVal interface.
//
// Interpretation: A FunVal represents the value of a lambda expression.

import java.util.Map;

interface FunVal extends ExpVal {


    // Returns the lambda expression from which this function was created.
    LambdaExp code();

    // Returns the environment that maps the free variables of that
    // lambda expression to their values.
    Map<String,ExpVal> environment();

}
