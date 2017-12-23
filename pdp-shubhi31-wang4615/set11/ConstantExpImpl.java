
import java.util.Map;

// Constructor template for ConstantExpImpl:
//   new ConstantExpImpl(value)
// Interpretation:
//   value is an ExpVal 
public class ConstantExpImpl extends ExpBase implements ConstantExp 
{
	private ExpVal value; // Value of the constant expression

	public ConstantExpImpl(ExpVal value) {
		this.value = value;
	}

	// Returns true because this object represents a constant.
	public boolean isConstant() {
		return true;
	}

	// Precondition: this Exp is for a constant.
	// Returns: a ConstantExp like this one.
    public ConstantExp asConstant() {

		return new ConstantExpImpl(this.value);
    }

    // Returns: the expression value of this constant Expression.
	public ExpVal value() {
	    return this.value;
	}


	// value method overloaded only to keep the constant value method
    // generic to other expressions value methods

	// Given: The environment for the expression
    // Where:  environment is always empty
    // Returns: the expression value of this constant Expression.
    public ExpVal value (Map<String,ExpVal> env) {
    	return value();
	}		
}