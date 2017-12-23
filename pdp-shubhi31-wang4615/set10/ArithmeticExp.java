// An ArithmeticExp is an object of any class that implements ArithmeticExp.
//
// Interpretation: An ArithmeticExp represents an expression of the form
//
//     <expr> <op> <expr>
//
// where <op> is one of the binary operators
//     <
//     =
//     >
//     +
//     -
//     *
interface ArithmeticExp extends Exp {

    // Returns the appropriate subexpression.
    Exp leftOperand();
    Exp rightOperand();

    // Returns the binary operation as one of the strings
    //     "LT"
    //     "EQ"
    //     "GT"
    //     "PLUS"
    //     "MINUS"
    //     "TIMES"
    String operation();
}
