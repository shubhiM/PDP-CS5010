import java.util.List;
import java.util.Map;
import java.util.HashMap;

// Constructor template for CallExpImpl:
//   new  CallExpImpl(operator, arguments)
// Interpretation:
//   operator is an identifier of the function being called.
//   argument is a list of expressions being called on.

public class CallExpImpl extends ExpBase implements CallExp {

    // operator is the identifier for the function called
    private Exp operator;
    // argument is a list of expressions being called on.
    private List<Exp> arguments;

    public CallExpImpl(Exp operator, List<Exp> arguments) {
        this.operator = operator;
        this.arguments = arguments;
    }

    // Returns true because this Exp is a CallExp.
    public boolean isCall() {
        return true;
    }

    // Precondition: this Exp is for a call.
    // Returns: a CallExp like this one.
    public CallExp  asCall() {
        return new CallExpImpl(this.operator, this.arguments);
    }

    // Returns: the expression for the operator part of the call.
    public Exp operator() {
        return this.operator;
    }

    // Returns: the list of argument expressions.
    public List<Exp> arguments() {
        return this.arguments;
    }

    // Given: a Map that represents the environment for the program.
    // Returns: the val of this call expression.
    // Example:  plus2 (x) = x + 2
    //   Exp call = Asts.callExp(
    //       Asts.identifierExp("plus2"),
    //       Asts.list(Asts.expVal(10)));
    //   call.value(env) ==> 12
    public ExpVal value (Map<String, ExpVal> progEnv) {

        Map<String, ExpVal> runtimeEnv = new HashMap<String, ExpVal>();
        List<String> formal;
        LambdaExp code;

        ExpVal ex = this.operator.value(progEnv);
 
        if (ex.isFunction()) {
            FunVal func = ex.asFunction();
            code = func.code(); 
            formal = code.formals();

            runtimeEnv.putAll(func.environment());

        } else {
            // cannot call a constant/arithmetic/call/if exp as a function.
            throw new RuntimeException("exp cannot be called."); 
        }
        // check function arity
        if (this.arguments.size() != formal.size()) {
            throw new RuntimeException("wrong number of arguments.");
        }
        // associate lambda formals with argument values and store them 
        // in runtimeEnv
        for(int i = 0; i < this.arguments.size(); i++) {
            ExpVal val = this.arguments.get(i).value(progEnv);
            runtimeEnv.put(formal.get(i), val);
        }
        // get the value of the function call by 
        // call the value method of the code's body with runtimeEnv.
        return code.body().value(runtimeEnv);

    } 
}
