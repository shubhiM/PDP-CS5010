// An ExpVal is an object of any class that implements the ExpVal interface.
//
// Interpretation: An ExpVal represents the value of an expression.

import java.util.Map;

interface ExpVal {

    // Returns true iff this ExpVal is a boolean, integer, or
    // function (respectively).
    boolean isBoolean();
    boolean isInteger();
    boolean isFunction();

    // Precondition: the corresponding predicate above is true.
    // Returns this.
    // (These methods amount should eliminate most casts.)
    boolean asBoolean();
    long asInteger();
    FunVal asFunction();
}
