
import java.util.List;

// AstBase is an abstract class which provides default implementations
// for classes that implements the Ast interface;

public abstract class AstBase implements Ast {

    // Returns false as default behavior for this method
    public boolean isPgm() {
    	return false;
    }

    // Returns false as default behavior for this method
    public boolean isDef() {
    	return false;
    }

    // Returns false as default behavior for this method
    public boolean isExp() {
    	return false;
    }

    // Returns null as default behavior for this method
    public List<Def> asPgm() {
    	return null;
    }

    // Returns null as default behavior for this method
    public Def asDef() {
    	return null;
    }

    // Returns null as default behavior for this method
    public Exp asExp() {
    	return null;
    }
}