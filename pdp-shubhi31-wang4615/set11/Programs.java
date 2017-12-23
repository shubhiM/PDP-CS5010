import java.util.*;

// Programs class encapsulates logic to run the program with the provided list
// of inputs.

public class Programs {
    // Runs the ps11 program found in the file named on the command line
    // on the integer inputs that follow its name on the command line,
    // printing the result computed by the program.
    //
    // Example:
    //     % java Programs sieve.ps11 2 100
    //     25
    public static void main (String[] args) {
        if(args.length < 1) {
            throw new RuntimeException("not enough arguments");
        }

        // get the program by calling readPgm and parsePgm
        String filename = args[0];
        List<Def> defs = parsePgm(readPgm(filename));
        
        // get inputs as integers
        List<ExpVal> inputs = new ArrayList<ExpVal>();
        for(int i = 1; i < args.length; i++) {
            Integer input = Integer.parseInt(args[i]);
            inputs.add(Asts.expVal(input));
        }

        // run the program and print result to screen
        System.out.println(Programs.run(defs, inputs).asInteger());
    }
    
    // Reads the ps11 program found in the file named by the given string
    // and returns the set of all variable names that occur free within
    // the program.
    //
    // Examples:
    //     Programs.undefined ("church.ps11")    // returns an empty set
    //     Programs.undefined ("bad.ps11")       // returns { "x", "z" }
    //
    //   where bad.ps11 is a file containing:
    // 
    //     f (x, y) g (x, y) (y, z);
    //     g (z, y) if 3 > 4 then x else f
    public static Set<String> undefined (String filename) { 
        Set<String> result = new HashSet<String>();
        
        // read and parse the program.
        List<Def> pgm = parsePgm(readPgm(filename));

        // prepare the program environment.
        Set<String> env = new HashSet<String>();
        for (int i = 0; i < pgm.size(); i++) {
            Def def = pgm.get(i);            
            env.add(def.lhs());
        }

        // check each definition in the program.
        for (int i = 0; i < pgm.size(); i++) {
            Def def = pgm.get(i);            
            checkExp(def.rhs(), env, result);
        }

        return result;
    }

    // checkExp checks which variable name in the given expression is undifined
    //   and add the variable name to result.
    // Given: an expression, an environment, and a set that stores the result.
    // Returns: void.
    private static void checkExp(Exp exp, Set<String> env, Set<String> result) {
        if(exp.isIdentifier()) {
            IdentifierExp id = exp.asIdentifier();
            if(!env.contains(id.name())) 
                result.add(id.name());
        } else if(exp.isConstant()) {
            // Do nothing
        } else if(exp.isIf()) {
            IfExp ifExp = exp.asIf();
            checkExp(Arrays.asList(ifExp.testPart(), ifExp.thenPart(), ifExp.elsePart()), 
                    env, 
                    result);
        } else if(exp.isArithmetic()) {
            ArithmeticExp ariExp = exp.asArithmetic();
            checkExp(Arrays.asList(ariExp.leftOperand(),ariExp.rightOperand()),
                     env, 
                     result);
        } else if(exp.isLambda()) {
            LambdaExp lambda = exp.asLambda();
            Set<String> env1 = new HashSet<String>(lambda.formals());
            env1.addAll(env);
            checkExp(lambda.body(), env1, result);
        } else if(exp.isCall()) {
            CallExp call = exp.asCall();
            checkExp(call.arguments(), env, result);
            checkExp(call.operator(), env, result);
        } else {
            throw new RuntimeException("undefined expression.");
        }
        return;
    }

    // checkExp that accepts a list of expressions instead one expression;
    private static void checkExp(List<Exp> exps, Set<String> env, Set<String> result) {
        for(Exp exp: exps) {
            checkExp(exp, env, result);
        }
    }

    // helper function to read a program from file.
    // Given: file name.
    // Returns: the program as a String.
    private static String readPgm (String filename) {
        return Scanner.readPgm (filename);
    }
    
    // helper function to parse the program.
    // Given: a String that represents a program.
    // Return: the given program as a list of definitions.
    private static List<Def> parsePgm (String pgm0) {
        return Scanner.parsePgm (pgm0);
    }

    // Given: List of definitions for the program and the list of inputs as
    // values
    // Returns: the resulting value of program execution.
    // Details : The first entry in the list is the main entry point of the
    // program and list of inputs are all the inputs to be used by this main
    // definition. The following definitions are all set in the environment
    // to be used by the main function as required
    public static ExpVal run (List<Def> pgm, List<ExpVal> inputs) {

        // env is the program environment
        Map<String, ExpVal> env = new HashMap<String, ExpVal>();

     	// Creating environment for program execution
        // For each definition in the environment
        // key is the identifier for that definition (lhs)
        // value is the value of expression (rhs) for that
        // definition.
     	for (int i = 0; i < pgm.size(); i++) {
     		Def def = pgm.get(i);
     		env.put(def.lhs(), def.rhs().value(env));
     	}

     	// the first definition of a program must define a function,
        // which is the entry point of this program
     	Exp main = Asts.identifierExp(pgm.get(0).lhs());
     	List<Exp> arguments = new LinkedList<Exp>();
     	for(ExpVal val : inputs) {
            arguments.add(Asts.constantExp(val));
     	}
     	Exp call = Asts.callExp(main, arguments);
     	return call.value(env);
    }
}
