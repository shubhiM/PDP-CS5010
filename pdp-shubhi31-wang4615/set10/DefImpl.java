// Constructor template for DefImpl:
//   new DefImpl(id, rhs)
// Interpretation:
//   id is a Strings that represents name of the definition.
//   rhs is the expression associated with this definition.
public class DefImpl extends AstBase implements Def {

    //id is a Strings that represents name of the definition.
    private String id;

    // rhs is the expression associated with this definition.
    private Exp rhs;

	public DefImpl(String id, Exp rhs) {
		this.id = id;
		this.rhs = rhs;
	}

    // Returns: true because this object represents a definition.
    public boolean isDef() {
    	return true;
    }

    // Precondition: this Ast is for a definition.
    // Returns: a representation of that definition.
    public Def asDef() {
    	return new DefImpl(this.id, this.rhs);
    }

    // Precondition: this Ast is for a definition.
    // Returns: the left hand side of this definition,
    public String lhs() {
    	return this.id;
    }
    
    // Precondition: this Ast is for a definition.
    // Returns the right hand side of this definition,
    public Exp rhs() {
   		return this.rhs;
    }

}