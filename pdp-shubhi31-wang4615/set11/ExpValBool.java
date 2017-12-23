// Constructor template for ExpValBool:
//   new  ExpValBool(bool)
// Interpretation:
//   bool is an boolean value.
public class ExpValBool extends ExpValBase {

   private boolean value; //   A boolean value.

   public ExpValBool(boolean bool) {
       this.value = bool;
   }

   // Returns true iff the instance is of this class
   public boolean isBoolean() {
       return true;
   }

   // Precondition: the corresponding predicate above is true.
   // Returns value for this.
   public boolean asBoolean() {
       return this.value;
   }

}
