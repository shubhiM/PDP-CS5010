// An IfExp is an object of any class that implements IfExp.
//
// Interpretation: An IfExp represents an if expression.

interface IfExp extends Exp {

    // Returns the appropriate part of this if expression.

    Exp testPart();
    Exp thenPart();
    Exp elsePart();
}
