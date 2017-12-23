// Action routines, called by parser.

import java.util.*;

class Actions {

    private Scanner scanner;    // the lexical analyzer

    Actions (Scanner scanner) {
        this.scanner = scanner;
    }

    /*  Action routines.  */

    Ast identity(Ast x) {
        return x;
    }

    // lists of abstract syntax trees

    Ast mkEmpty() {
        return new MyList<Ast>();
    }

    Ast mkEmpty0() {
        return new MyList<Ast>("Def");    // a list of definitions
    }

    @SuppressWarnings("unchecked")
    Ast cons(Ast xexpr_1, Ast xexprs2_2) {
        List<Ast> rest = (List<Ast>) xexprs2_2;
        rest.add(0, xexpr_1);
        return xexprs2_2;
    }

    // the straightforward action routines

    Ast mkDef(Ast xid_1, Ast xdefn2_2) {
        String id = xid_1.asExp().asIdentifier().name();
        return Asts.def (id, xdefn2_2.asExp());
    }
    Ast mkBool() {
        boolean b = scanner.tokenValue().equals("true");
        return Asts.constantExp (Asts.expVal (b));
    }
    Ast mkInt() {
        String s = scanner.tokenValue();
        long k = Long.parseLong (s);
        return Asts.constantExp (Asts.expVal (k));
    }
    Ast mkId() {
        String id = scanner.tokenValue();
        return Asts.identifierExp (id);
    }
    Ast mkIf(Ast xexpr_1, Ast xexpr_2, Ast xexpr_3) {
        return Asts.ifExp (xexpr_1.asExp(), xexpr_2.asExp(), xexpr_3.asExp());
    }

    @SuppressWarnings("unchecked")
    Ast mkLambda(Ast xformals_1, Ast xexpr_2) {
        List<Ast> asts = (List<Ast>) xformals_1;
        MyList<String> formals = new MyList<String>();
        for (Ast ast : asts) {
            formals.add (ast.asExp().asIdentifier().name());
        }
        formals.becomeImmutable();
        return Asts.lambdaExp (formals, xexpr_2.asExp());
    }

    // routines that deal with left factoring or left association

    // The first argument is an Exp.
    // The second argument is one of:
    //     an ArithmeticExp returned by mkNone
    //     a PartialCall

    Ast mkCall2(Ast xexpr_1, Ast xexpr_2) {
	if (xexpr_2.isExp())
	    return xexpr_1;
	else {
	    Exp expr1 = xexpr_1.asExp();
	    PartialCall pcall = (PartialCall) xexpr_2;
	    List<Exp> args = pcall.args();
	    Ast moreargs = pcall.moreargs();
	    return mkCall2 (Asts.callExp (expr1, args), moreargs);
	}
    }

    // The first argument is a List<Exp> giving the arguments for a call.
    // The second argument is one of:
    //     an ArithmeticExp returned by mkNone
    //     a PartialCall

    Ast mkCalls(Ast args, Ast moreargs) {
	return new PartialCall (args, moreargs);
    }

    // The following rather strange invariants avoid introducing
    // various temporary forms of abstract syntax tree.
    //
    // The second argument is a partially constructed Ast, one of:
    //     a CallExp c whose operator is expr3
    //         meaning the result should be
    //         leftAssoc (Asts.callExp (expr1, c.arguments()), expr3)
    //     an ArithmeticExp whose operands are both null
    //         meaning expr1 is all there is
    //     an ArithmeticExp whose left operand is expr3
    //         and whose operation is op
    //         and whose right operand is expr2,
    //         meaning the result should be
    //         leftAssoc (Ast.arithmeticExp (expr1, op, expr2), expr3)

    Ast leftAssoc(Ast expr1, Ast partial0) {
        Exp partial1 = partial0.asExp();
        if (partial1.isArithmetic()) {
            ArithmeticExp partial = partial1.asArithmetic();
            Ast expr3 = partial.leftOperand();
            Ast expr2 = partial.rightOperand();
            String op = partial.operation();
            if (expr2 == null && expr3 == null)
                return expr1;
            else
                return leftAssoc (Asts.arithmeticExp (expr1.asExp(),
                                                      op,
                                                      expr2.asExp()),
                                  expr3);
        }
        else {
            CallExp partial = partial1.asCall();
            Exp expr3 = partial.operator();
            List<Exp> args = partial.arguments();
            return leftAssoc (Asts.callExp (expr1.asExp(), args), expr3);
        }
    }
    Ast mkNone() {
        return Asts.arithmeticExp (null, "NONE", null);
    }
    Ast mkPartial(Ast xmulop_1, Ast xfactor_2, Ast xterm2_3) {
        Exp expr2 = xfactor_2.asExp();
        Exp expr3 = xterm2_3.asExp();
        String op = xmulop_1.asExp().asArithmetic().operation();
        // see comment above
        return Asts.arithmeticExp (expr3, op, expr2);
    }
    Ast mkPartial1(Ast xrelop_1, Ast xarith_2) {
        return mkPartial (xrelop_1, xarith_2, mkNone());
    }

    // routines that deal with arithmetic operators
    // they return an ArithmeticExp
    //     whose left and right operands are both null

    Ast mkEQ() {
        return Asts.arithmeticExp (null, "EQ", null);
    }
    Ast mkGT() {
        return Asts.arithmeticExp (null, "GT", null);
    }
    Ast mkLT() {
        return Asts.arithmeticExp (null, "LT", null);
    }
    Ast mkMINUS() {
        return Asts.arithmeticExp (null, "MINUS", null);
    }
    Ast mkPLUS() {
        return Asts.arithmeticExp (null, "PLUS", null);
    }
    Ast mkTIMES() {
        return Asts.arithmeticExp (null, "TIMES", null);
    }
}

// A partial call represents the arguments of a call,
// possibly with more arguments to be passed to the value of that call.

class PartialCall implements Ast {

    private List<Exp> args;
    private Ast moreargs;     // mkNone() or a PartialCall

    @SuppressWarnings("unchecked")
    PartialCall (Ast args, Ast moreargs) {
	this.args = (List<Exp>) args;
	this.moreargs = moreargs;
    }

    List<Exp> args () { return args; }
    Ast moreargs () { return moreargs; }

    ////////////////////////////////////////////////////////////////

    // Returns true iff this Ast is for a program, definition,
    // or expression, respectively
    
    public boolean isPgm() { return false; }
    public boolean isDef() { return false; }
    public boolean isExp() { return false; }
    
    // Precondition: this Ast is for a program.
    // Returns a representation of that program.
    
    public List<Def> asPgm() { throw new RuntimeException(); }
    
    // Precondition: this Ast is for a definition.
    // Returns a representation of that definition.
    
    public Def asDef() { throw new RuntimeException(); }
    
    // Precondition: this Ast is for an expression.
    // Returns a representation of that expression.
    
    public Exp asExp() { throw new RuntimeException(); }
}

// This implementation of List<E> implements Ast.
// Its lists are mutable at first, but can be made immutable
// by calling the becomeImmutable() method.

class MyList<E> extends ArrayList<E> implements Ast {

    private boolean isMutable = true;    // is this list still mutable?
    private String isWhat;               // a description of E

    MyList () {
        super();
    }

    MyList (String isWhat) {
	super();
	this.isWhat = isWhat;
    }

    // Changes this to an immutable List<E>.

    void becomeImmutable() {
        this.isMutable = false;
    }

    ////////////////////////////////////////////////////////////////

    // Returns true iff this Ast is for a program, definition,
    // or expression, respectively
    
    public boolean isPgm() { return isWhat.equals("Def"); }
    public boolean isDef() { return false; }
    public boolean isExp() { return false; }
    
    // Precondition: this Ast is for a program.
    // Returns a representation of that program.
    
    @SuppressWarnings("unchecked")
    public List<Def> asPgm() { return (List<Def>) this; }
    
    // Precondition: this Ast is for a definition.
    // Returns a representation of that definition.
    
    public Def asDef() { throw new RuntimeException(); }
    
    // Precondition: this Ast is for an expression.
    // Returns a representation of that expression.
    
    public Exp asExp() { throw new RuntimeException(); }

    ////////////////////////////////////////////////////////////////

    // These optional methods of List<E> are disabled if
    // becomeMutable() has been called.  Otherwise they
    // behave as in ArrayList<E>.

    public boolean add (E e) {
        if (isMutable)
            return super.add (e);
        else throw new UnsupportedOperationException();
    }

    public void add (int index, E element) {
        if (isMutable)
            super.add (index, element);
        else throw new UnsupportedOperationException();
    }

    public boolean addAll (Collection<? extends E> c) {
        if (isMutable)
            return super.addAll (c);
        else throw new UnsupportedOperationException();
    }

    public boolean addAll (int index, Collection<? extends E> c) {
        if (isMutable)
            return super.addAll (index, c);
        else throw new UnsupportedOperationException();
    }

    public void clear () {
        if (isMutable)
            super.clear ();
        else throw new UnsupportedOperationException();
    }

    public E remove (int index) {
        if (isMutable)
            return super.remove (index);
        else throw new UnsupportedOperationException();
    }

    public boolean remove (Object o) {
        if (isMutable)
            return super.remove (o);
        else throw new UnsupportedOperationException();
    }

    public boolean removeAll (Collection<?> c) {
        if (isMutable)
            return super.removeAll (c);
        else throw new UnsupportedOperationException();
    }

    public boolean retainAll (Collection<?> c) {
        if (isMutable)
            return super.retainAll (c);
        else throw new UnsupportedOperationException();
    }

    public E set (int index, E element) {
        if (isMutable)
            return super.set (index, element);
        else throw new UnsupportedOperationException();
    }
}
