import java.util.Map;

// Constructor template for IdentifierExpImpl:
//   new IdentifierExpImpl(name)
// Interpretation:
//   name is a String that is used as an identifier  
class IdentifierExpImpl extends ExpBase implements IdentifierExp {

    private String name; // name is a String that is used as an identifier

    IdentifierExpImpl(String name) {
        this.name = name;
    }

    // Returns: true because this object is an identifier
    public boolean isIdentifier() {
        return true;
    }

    // Precondition: this Exp is for an identifier.
    // Returns: an IdentifierExp like this one.
    public IdentifierExp asIdentifier() {
        return new IdentifierExpImpl(this.name) ;
    }

    // Precondition: this Exp is for an identifier.
    // Returns: the name of this identifier
    public String name() {
        return this.name;
    }

    // Given: a environment.
    // Returns: the value of this identifier, by retrieving the value
    //   from the environment.
    public ExpVal value (Map<String,ExpVal> env) {

        return env.get(this.name);
    }

}