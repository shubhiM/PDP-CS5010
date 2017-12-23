// Constructor template for ExpValInt:
//   new  ExpValInt(value)
// Interpretation:
//   value is a long integer. 

public class ExpValInt extends ExpValBase {

    private long value; // A long integer value.

    public ExpValInt(long value) {
        this.value = value;
    }

    // Returns true iff the instance is of this class
    public boolean isInteger() {
        return true;
    }

    // Precondition: the corresponding predicate above is true.
    // Returns value for this.
    public long asInteger() {
        return this.value;
    }

}