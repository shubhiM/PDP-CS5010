
import java.util.Map;

// Constructor template for FunValImpl:
//   new  FunValImpl(exp, env)
// Interpretation:
//   a function is a lambda expression with an environment. 
public class FunValImpl extends ExpValBase implements FunVal {

    private LambdaExp exp; // Lambda Expression
	private Map<String, ExpVal> env; // Environment

	public FunValImpl(LambdaExp exp, Map<String,ExpVal> env) {
		this.exp = exp;
		this.env = env;
	}
    
    // Returns: true because this ExpVal is a FunVal.
	public boolean isFunction() {
		return true;
	}
	
	// Preconditions: this ExpVal is a FunVal.
    // Returns: a FunVal like this one.
	public FunVal asFunction() {
		return new FunValImpl(this.exp, this.env);
	}


    // Returns: the lambda expression from which this function was created.
    public LambdaExp code() {
    	return this.exp;
    }

    // Returns: the environment that maps the free variables of that
    // lambda expression to their values.
    public Map<String,ExpVal> environment() {
    	return this.env;
    }
}
