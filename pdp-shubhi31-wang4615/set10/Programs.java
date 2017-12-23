import java.util.*;

// Programs class encapsulates logic to run the program with the provided list
// of inputs.

public class Programs {


    // Given: List of definitions for the program and the list of inputs as
    // values
    // Returns: the resulting value of program execution.
    // Details : The first entry in the list is the main entry point of the
    // program and list of inputs are all the inputs to be used by this main
    // definition. The following definitions are all set in the environment
    // to be used by the main function as required

    public static ExpVal run (List<Def> pgm, List<ExpVal> inputs) {

        // env0 is used to pass an empty environment to evaluate expressions
        Map<String, ExpVal> env0 = new HashMap<String, ExpVal>();

        // env is the program environment
        Map<String, ExpVal> env = new HashMap<String, ExpVal>();

     	// Creating environment for program execution
        // For each definition in the environment
        // key is the identifier for that definition (lhs)
        // value is the value of expression (rhs) for that
        // definition.

     	for (int i = 0; i < pgm.size(); i++) {
     		Def def = pgm.get(i);
     		env.put(def.lhs(), def.rhs().value(env0));
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
