
// ExpBase is an abstract class which provides default implementations
// for classes that implements the ExpVal interface;

public abstract class ExpValBase implements ExpVal {

	// Returns false irrespective of ExpVal is being a boolean, integer, or
    // function (respectively).
    public boolean isBoolean() {
    	return false;
	};
	public boolean isInteger() {
    	return false;
	}
    public boolean isFunction() {
    	return false;
	}

    // Precondition: the corresponding predicate above is true.
    // Returns default values for these methods.
    public boolean asBoolean() {
    	return false;
	}
    public long asInteger() {
    	return 0;
	}
    public FunVal asFunction() {
    	return null;
	}
} 
