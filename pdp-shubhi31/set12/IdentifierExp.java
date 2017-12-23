// An IdentifierExp is an object of any class that implements IdentifierExp.
//
// Interpretation: A IdentifierExp represents an identifier (or variable).

interface IdentifierExp extends Exp {

    // Returns the name of this identifier.

    String name();
}
