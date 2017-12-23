
import java.util.List;

public class PgmImpl extends AstBase {

    private List<Def> defs;
    
    public PgmImpl(List<Def> defs) {
        this.defs = defs;
    }
    public boolean isPgm() {
    	return true;
    }

    // Returns null as default behavior for this method
    public List<Def> asPgm() {
    	return this.defs;
    }

}