import java.util.Map;

// ArithmeticExpImpl is essentially an implementation of ArithmeticExp

// Constructor template for ArithmeticExpImpl:
//   new ArithmeticExpImpl(left, operator, right)
// Interpretation:
//   left is the expression on the left side of the operator.
//   operator is a String that represents the operator.
//   right is the expression on the right side of the operator.


class ArithmeticExpImpl extends ExpBase implements ArithmeticExp {

    // Operator, can be one of ["LT", "EQ", "GT", "PLUS",  "MINUS", "TIMES"]
    private String operator;

    // Expression to the left side of the operator
    private Exp left;

    // Expression to the right side of the operator
    private Exp right;



    // constructor for the ArithmeticExpImpl class
    // Assigns values to the left expression, operator and right expression
    public ArithmeticExpImpl(Exp left, String operator, Exp right) {
        this.left = left;
        this.right = right;
        this.operator = operator;
    }

    // Returns the left operand;
    public Exp leftOperand() {
        return this.left;
    }
    
    // Returns the right operand;
    public Exp rightOperand() {
        return this.right;
    }

    // Returns the binary operation as one of the strings;
    // "LT", "EQ", "GT", "PLUS",  "MINUS", "TIMES"
    public String operation() {
        return this.operator;
    }

    // Returns true iff this is an arithmetic expression;
    public boolean isArithmetic() {
        return true;
    }


    // Returns: A new arithmetic expression which left side expression,
    // operator and right side expression.
    public ArithmeticExp asArithmetic() {
        return new ArithmeticExpImpl(this.left, this.operator, this.right);
    }



    // Given: an environment which is a hash map consisting of keys as strings
    // and values as ExpVal objects.
    // Returns: An ExpVal that represents the value of the given arithmetic
    // expression in the given environment.

    // Examples:
    //  Sample Expression : 1 - 2
    //    ArithmeticExp sample = Asts.arithmeticExp(
    //            Asts.constantExp(Asts.expVal(1)),
    //            "MINUS",
    //            Asts.constantExp(Asts.expVal(2)));
    // environment for sample
    //    HashMap<String, ExpVal> env = new HashMap<>();
    //  sample.value(env) == Asts.expVal(-1)

    // Strategy: Cases based on the operator value
    public ExpVal value (Map<String,ExpVal> env) {
        ExpVal leftValue = this.left.value(env);
        ExpVal rightValue = this.right.value(env);

        if(leftValue.isInteger() && rightValue.isInteger()) {
            if(this.operator.equals("LT")) {
                return Asts.expVal(
                        leftValue.asInteger() < rightValue.asInteger());
            } else if(this.operator.equals("GT")) {
                return Asts.expVal(
                        leftValue.asInteger() > rightValue.asInteger());
            }  else if(this.operator.equals("EQ")) {
                return Asts.expVal(
                        leftValue.asInteger() == rightValue.asInteger());
            } else if(this.operator.equals("PLUS")) {
                return Asts.expVal(
                        leftValue.asInteger() +  rightValue.asInteger());
            } else if(this.operator.equals("MINUS")) {
                return Asts.expVal(
                        leftValue.asInteger() - rightValue.asInteger());
            } else if(this.operator.equals("TIMES")) {
                return Asts.expVal(
                        leftValue.asInteger() *  rightValue.asInteger());
            }
        } 
        throw new RuntimeException();
    }
}