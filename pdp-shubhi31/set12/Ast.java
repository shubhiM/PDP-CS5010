// An Ast is an object of any class that implements the Ast interface.
//
// Interpretation: An Ast represents the abstract syntax tree
// for some part of a source program.
//
// This abstract data type simplies the parser,
// but should not be used by the type checker or interpreter.

import java.util.List;

interface Ast {

    // Returns true iff this Ast is for a program, definition,
    // or expression, respectively
    boolean isPgm();
    boolean isDef();
    boolean isExp();

    // Precondition: this Ast is for a program.
    // Returns a representation of that program.
    List<Def> asPgm();

    // Precondition: this Ast is for a definition.
    // Returns a representation of that definition.
    Def asDef();

    // Precondition: this Ast is for an expression.
    // Returns a representation of that expression.
    Exp asExp();
}
