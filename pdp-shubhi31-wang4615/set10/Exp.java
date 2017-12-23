// An Exp is an object of any class that implements the Exp interface.
//
// Interpretation: An Exp represents an expression of a source program.

import java.util.Map;

interface Exp extends Ast {

    // Returns true iff this Exp is a constant, identifier,
    // lambda, arithmetic, call, or if expression (respectively).
    boolean isConstant();
    boolean isIdentifier();
    boolean isLambda();
    boolean isArithmetic();
    boolean isCall();
    boolean isIf();

    // Precondition: the corresponding predicate above is true.
    // Returns this.
    // (These methods amount should eliminate most casts.)
    ConstantExp   asConstant();
    IdentifierExp asIdentifier();
    LambdaExp     asLambda();
    ArithmeticExp asArithmetic();
    CallExp       asCall();
    IfExp         asIf();

    // Returns the value of this expression when its free variables
    //     have the values associated with them in the given Map.
    // May run forever if this expression has no value.
    // May throw a RuntimeException if some free variable of this
    //     expression is not a key of the given Map or if a type
    //     error is encountered during computation of the value.
    ExpVal value (Map<String,ExpVal> env);
}
