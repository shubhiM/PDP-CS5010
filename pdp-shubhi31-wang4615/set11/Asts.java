import java.util.List;
import java.util.Map;
import java.util.LinkedList;


// Asts is the factory class which encapsulates factory methods for all types
// of expressions, expression values and other helper methods.

public class Asts {

	// Static factory methods for Def
	// Given: the left and right hand side of the definition.
	// Returns: a Def object with the given left and right hand size.
	public static Def def (String id1, Exp rhs) { 
		return new DefImpl(id1, rhs);
	}

	// Static factory methods for Exp
	// Given: an Exp e1, a String that represents an operator, and an Exp e2.
	// Returns: an ArithmeticExp object representing e1 op e2.
	public static ArithmeticExp arithmeticExp (Exp e1, String op, Exp e2) {
		return new ArithmeticExpImpl(e1, op, e2);
	}
	
	// Static factory methods for CallExp
	// Given: an Exp operator that represents the function and a list of Exp
	//  that represents the arguments.
	// Returns a CallExp object with the given operator and operand expressions.
	public static CallExp callExp (Exp operator, List<Exp> operands) { 
		return new CallExpImpl(operator, operands);
 	}

	// Static factory methods for ConstantExp
 	// Given: a ExpVal value.
	// Returns a ConstantExp object with the given value.
	public static ConstantExp constantExp (ExpVal value) { 
		return new ConstantExpImpl(value);
	}

	// Static factory methods for IdentifierExp
	// Given: a String id.
	// Returns: an IdentifierExp object with the given identifier name.
	public static IdentifierExp identifierExp (String id) {
		return new IdentifierExpImpl(id);
	}

	// Static factory methods for IfExp
	// Given: the test, then and else part of the if expression.
	// Returns: an IfExp object with the given components.

	public static IfExp ifExp (Exp testPart, Exp thenPart, Exp elsePart) {
	    return new IfExpImpl(testPart, thenPart, elsePart);
    }

	// Static factory methods for LambdaExp
	// Given: a list of string that reprsents the formals and a body expression.
	// Returns: a LambdaExp object with the given formals and body.
	public static LambdaExp lambdaExp (List<String> formals, Exp body) {
	    return new LambdaExpImpl(formals, body);
    }

	// Static factory methods for ExpVal
	// Given: a boolean.
	// Returns: a value encapsulating the given boolean.
	public static ExpVal expVal (boolean b) { 

	    return new ExpValBool(b);
	}

	// Static factory methods for ExpVal
	// Given: a long integer.
	// Returns: a value encapsulating the given (long) integer.
	public static ExpVal expVal (long n) { 
	 	return new ExpValInt(n);

	}
	
	// Static factory methods for ExpVal
	// Given: a LambdaExp exp and an environment.
	// Returns: a function(FunVal) encapsulating the given lambda expression
    //   and environment.
	public static FunVal expVal (LambdaExp exp, Map<String,ExpVal> env) {
		return new FunValImpl(exp, env);
	}


	// Static methods for creating short lists
	// Given: one element x of class X.
	// Returns: a List of X objects with the given x.
	public static <X> List<X> list (X x1) { 
		List<X> result = new LinkedList<X>();
		result.add(x1);
		return result;
	}

    // Static methods for creating short lists
    // Given: two elements x of class X.
    // Returns: a List of X objects with the given x's.
	public static <X> List<X> list (X x1, X x2) { 		
		List<X> result = new LinkedList<X>();
		result.add(x1);
		result.add(x2);
		return result; 
	}

    // Static methods for creating short lists
    // Given: three elements x of class X.
    // Returns: a List of X objects with the given x's.
	public static <X> List<X> list (X x1, X x2, X x3) { 		
		List<X> result = new LinkedList<X>();
		result.add(x1);
		result.add(x2);
		result.add(x3);
		return result; 
	}

    // Static methods for creating short lists
    // Given: four elements x of class X.
    // Returns: a List of X objects with the given x's.
	public static <X> List<X> list (X x1, X x2, X x3, X x4) { 		
		List<X> result = new LinkedList<X>();
		result.add(x1);
		result.add(x2);
		result.add(x3);
		result.add(x4);
		return result;  
	}

}
