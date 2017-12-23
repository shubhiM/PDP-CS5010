
import java.util.Map;

// ExpBase is an abstract class which provides default implementations
// for classes that implements the Exp interface;

public abstract class ExpBase extends AstBase implements Exp {

    // Returns default value as true.
	public boolean isExp() {
    	return true;
    }


    // Returns this expression
    public Exp asExp() {
        return this;
    }

    // Returns default value as true.
    public boolean isConstant() {
    	return false;
    }
    // Returns default value as true.
    public boolean isIdentifier() {
    	return false;
    }
    // Returns default value as true.
    public boolean isLambda() {
    	return false;
    }
    // Returns default value as true.
    public boolean isArithmetic() {
    	return false;
    }
    // Returns default value as true.
    public boolean isCall() {
    	return false;
    }
    // Returns default value as true.
    public boolean isIf() {
    	return false;
    }


    // Returns default value as null.
    public ConstantExp asConstant() {
    	return null;
    }
    // Returns default value as null.
    public IdentifierExp asIdentifier() {
    	return null;
    }
    // Returns default value as null.
    public LambdaExp asLambda() {
    	return null;
    }
    // Returns default value as null.
    public ArithmeticExp asArithmetic() {
    	return null;
    }
    // Returns default value as null.
    public CallExp asCall() {
    	return null;
    }
    // Returns default value as null.
    public IfExp asIf() {
    	return null;
    }


    // Returns the value of this expression when its free variables
    //     have the values associated with them in the given Map.
    // May run forever if this expression has no value.
    // May throw a RuntimeException if some free variable of this
    //     expression is not a key of the given Map or if a type
    //     error is encountered during computation of the value.
    abstract public ExpVal value (Map<String,ExpVal> env);
}