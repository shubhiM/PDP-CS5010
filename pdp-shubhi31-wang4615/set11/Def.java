// A Def is an object of any class that implements the Def interface.
//
// Interpretation: A Def represents one definition of the source program.

interface Def extends Ast {

    // Returns the left hand side of this definition,
    // which will be an identifier represented as a String.

    String lhs();

    // Returns the right hand side of this definition,
    // which will be a ConstantExp or a LambdaExp.

    Exp rhs();
}
