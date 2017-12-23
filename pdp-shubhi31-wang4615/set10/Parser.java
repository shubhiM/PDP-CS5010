//package parser;

public class Parser {

    public Parser (Scanner s, Actions a) {
        scanner = s;
        actions = a;
    }

    private Scanner scanner;
    private Actions actions;

    /*  Tokens.  */

    public static final int zboolean = 1;
    public static final int zcomma = 2;
    public static final int zelse = 3;
    public static final int zeof = 4;
    public static final int zeq = 5;
    public static final int zgt = 6;
    public static final int zid = 7;
    public static final int zif = 8;
    public static final int zinteger = 9;
    public static final int zlambda = 10;
    public static final int zlparen = 11;
    public static final int zlt = 12;
    public static final int zminus = 13;
    public static final int zplus = 14;
    public static final int zrparen = 15;
    public static final int zsemi = 16;
    public static final int zthen = 17;
    public static final int ztimes = 18;
    public static final int zz1 = 19;

    /*  Nonterminals.  */

    public static final int yaddop = 1;
    public static final int yarith = 2;
    public static final int yarith2 = 3;
    public static final int yconst = 4;
    public static final int ydefn = 5;
    public static final int ydefn2 = 6;
    public static final int yexpr = 7;
    public static final int yexpr2 = 8;
    public static final int yexprs = 9;
    public static final int yexprs2 = 10;
    public static final int yfactor = 11;
    public static final int yfactor2 = 12;
    public static final int yfactor3 = 13;
    public static final int yformals = 14;
    public static final int yformals2 = 15;
    public static final int yid = 16;
    public static final int yif = 17;
    public static final int ylambda = 18;
    public static final int ylambda2 = 19;
    public static final int ymulop = 20;
    public static final int ypgm = 21;
    public static final int ypgm2 = 22;
    public static final int yrelop = 23;
    public static final int yterm = 24;
    public static final int yterm2 = 25;

    /*  Scanner routines.  */

    private final int nextToken () { return scanner.nextToken(); }
    private final void consumeToken () { scanner.consumeToken(); }

    /*  Error routine.  */

    private final Ast parseError (int x, Tokens y) {
        return scanner.parseError (x, y);
    }

    /*  Action routines.  */

    private final Ast cons(Ast xexpr_1, Ast xexprs2_2) {
        return actions.cons (xexpr_1, xexprs2_2);
    }
    private final Ast identity(Ast xexprs_1) {
        return actions.identity (xexprs_1);
    }
    private final Ast leftAssoc(Ast xfactor_1, Ast xterm2_2) {
        return actions.leftAssoc (xfactor_1, xterm2_2);
    }
    private final Ast mkBool() {
        return actions.mkBool ();
    }
    private final Ast mkCall2(Ast xlambda2_1, Ast xfactor2_2) {
        return actions.mkCall2 (xlambda2_1, xfactor2_2);
    }
    private final Ast mkCalls(Ast xexprs_1, Ast xfactor2_2) {
        return actions.mkCalls (xexprs_1, xfactor2_2);
    }
    private final Ast mkDef(Ast xid_1, Ast xdefn2_2) {
        return actions.mkDef (xid_1, xdefn2_2);
    }
    private final Ast mkEQ() {
        return actions.mkEQ ();
    }
    private final Ast mkEmpty() {
        return actions.mkEmpty ();
    }
    private final Ast mkEmpty0() {
        return actions.mkEmpty0 ();
    }
    private final Ast mkGT() {
        return actions.mkGT ();
    }
    private final Ast mkId() {
        return actions.mkId ();
    }
    private final Ast mkIf(Ast xexpr_1, Ast xexpr_2, Ast xexpr_3) {
        return actions.mkIf (xexpr_1, xexpr_2, xexpr_3);
    }
    private final Ast mkInt() {
        return actions.mkInt ();
    }
    private final Ast mkLT() {
        return actions.mkLT ();
    }
    private final Ast mkLambda(Ast xformals_1, Ast xexpr_2) {
        return actions.mkLambda (xformals_1, xexpr_2);
    }
    private final Ast mkMINUS() {
        return actions.mkMINUS ();
    }
    private final Ast mkNone() {
        return actions.mkNone ();
    }
    private final Ast mkPLUS() {
        return actions.mkPLUS ();
    }
    private final Ast mkPartial(Ast xmulop_1, Ast xfactor_2, Ast xterm2_3) {
        return actions.mkPartial (xmulop_1, xfactor_2, xterm2_3);
    }
    private final Ast mkPartial1(Ast xrelop_1, Ast xarith_2) {
        return actions.mkPartial1 (xrelop_1, xarith_2);
    }
    private final Ast mkTIMES() {
        return actions.mkTIMES ();
    }

    /*  Parsing routines.  */

    public Ast parsePgm () {
        Ast ast1, ast2;
        switch (nextToken()) {
            case zid:
                ast1 = parseDefn();
                ast2 = parsePgm2();
                return cons (ast1, ast2);
            default:
                return parseError (ypgm, new Tokens (zid, null));
        }
    } /* end of parsePgm */

    private Ast parsePgm2 () {
        Ast ast1;
        switch (nextToken()) {
            case zsemi:
                consumeToken();
                ast1 = parsePgm();
                return identity (ast1);
            case zeof:
                consumeToken();
                return mkEmpty0 ();
            default:
                return parseError (ypgm2, new Tokens (zeof, 
                        new Tokens (zsemi, null)));
        }
    } /* end of parsePgm2 */

    private Ast parseDefn () {
        Ast ast1, ast2;
        switch (nextToken()) {
            case zid:
                ast1 = parseId();
                ast2 = parseDefn2();
                return mkDef (ast1, ast2);
            default:
                return parseError (ydefn, new Tokens (zid, null));
        }
    } /* end of parseDefn */

    private Ast parseDefn2 () {
        Ast ast1, ast2;
        switch (nextToken()) {
            case zlparen:
                consumeToken();
                ast1 = parseFormals();
                if (nextToken() == zrparen) {
                    consumeToken();
                    ast2 = parseExpr();
                    return mkLambda (ast1, ast2);
                }
                else return parseError(ydefn2, new Tokens (zrparen, 
                        null));
            case zeq:
                consumeToken();
                ast1 = parseConst();
                return identity (ast1);
            default:
                return parseError (ydefn2, new Tokens (zeq, 
                        new Tokens (zlparen, null)));
        }
    } /* end of parseDefn2 */

    private Ast parseConst () {
        switch (nextToken()) {
            case zinteger:
                consumeToken();
                return mkInt ();
            case zboolean:
                consumeToken();
                return mkBool ();
            default:
                return parseError (yconst, new Tokens (zboolean, 
                        new Tokens (zinteger, null)));
        }
    } /* end of parseConst */

    private Ast parseLambda () {
        Ast ast1, ast2;
        switch (nextToken()) {
            case zlparen:
                consumeToken();
                if (nextToken() == zlambda) {
                    consumeToken();
                    if (nextToken() == zlparen) {
                        consumeToken();
                        ast1 = parseFormals();
                        if (nextToken() == zrparen) {
                            consumeToken();
                            ast2 = parseExpr();
                            if (nextToken() == zrparen) {
                                consumeToken();
                                return mkLambda (ast1, ast2);
                            }
                            else return parseError(ylambda, 
                        new Tokens (zrparen, null));
                        }
                        else return parseError(ylambda, 
                        new Tokens (zrparen, null));
                    }
                    else return parseError(ylambda, 
                        new Tokens (zlparen, null));
                }
                else return parseError(ylambda, new Tokens (zlambda, 
                        null));
            default:
                return parseError (ylambda, new Tokens (zlparen, null));
        }
    } /* end of parseLambda */

    private Ast parseLambda2 () {
        Ast ast1, ast2;
        switch (nextToken()) {
            case zlambda:
                consumeToken();
                if (nextToken() == zlparen) {
                    consumeToken();
                    ast1 = parseFormals();
                    if (nextToken() == zrparen) {
                        consumeToken();
                        ast2 = parseExpr();
                        if (nextToken() == zrparen) {
                            consumeToken();
                            return mkLambda (ast1, ast2);
                        }
                        else return parseError(ylambda2, 
                        new Tokens (zrparen, null));
                    }
                    else return parseError(ylambda2, 
                        new Tokens (zrparen, null));
                }
                else return parseError(ylambda2, new Tokens (zlparen, 
                        null));
            default:
                return parseError (ylambda2, new Tokens (zlambda, null));
        }
    } /* end of parseLambda2 */

    private Ast parseIf () {
        Ast ast1, ast2, ast3;
        switch (nextToken()) {
            case zif:
                consumeToken();
                ast1 = parseExpr();
                if (nextToken() == zthen) {
                    consumeToken();
                    ast2 = parseExpr();
                    if (nextToken() == zelse) {
                        consumeToken();
                        ast3 = parseExpr();
                        return mkIf (ast1, ast2, ast3);
                    }
                    else return parseError(yif, new Tokens (zelse, null));
                }
                else return parseError(yif, new Tokens (zthen, null));
            default:
                return parseError (yif, new Tokens (zif, null));
        }
    } /* end of parseIf */

    private Ast parseExpr () {
        Ast ast1, ast2;
        switch (nextToken()) {
            case zinteger:
            case zboolean:
            case zid:
            case zlparen:
                ast1 = parseArith();
                ast2 = parseExpr2();
                return leftAssoc (ast1, ast2);
            case zif:
                ast1 = parseIf();
                return identity (ast1);
            default:
                return parseError (yexpr, new Tokens (zboolean, 
                        new Tokens (zid, new Tokens (zif, 
                        new Tokens (zinteger, new Tokens (zlparen, 
                        null))))));
        }
    } /* end of parseExpr */

    private Ast parseExpr2 () {
        Ast ast1, ast2;
        switch (nextToken()) {
            case zlt:
            case zeq:
            case zgt:
                ast1 = parseRelop();
                ast2 = parseArith();
                return mkPartial1 (ast1, ast2);
            case zcomma:
            case zelse:
            case zthen:
            case zrparen:
            case zsemi:
            case zeof:
                return mkNone ();
            default:
                return parseError (yexpr2, new Tokens (zcomma, 
                        new Tokens (zelse, new Tokens (zeof, 
                        new Tokens (zeq, new Tokens (zgt, 
                        new Tokens (zlt, new Tokens (zrparen, 
                        new Tokens (zsemi, new Tokens (zthen, 
                        null))))))))));
        }
    } /* end of parseExpr2 */

    private Ast parseArith () {
        Ast ast1, ast2;
        switch (nextToken()) {
            case zlparen:
            case zid:
            case zboolean:
            case zinteger:
                ast1 = parseTerm();
                ast2 = parseArith2();
                return leftAssoc (ast1, ast2);
            default:
                return parseError (yarith, new Tokens (zboolean, 
                        new Tokens (zid, new Tokens (zinteger, 
                        new Tokens (zlparen, null)))));
        }
    } /* end of parseArith */

    private Ast parseArith2 () {
        Ast ast1, ast2, ast3;
        switch (nextToken()) {
            case zplus:
            case zminus:
                ast1 = parseAddop();
                ast2 = parseTerm();
                ast3 = parseArith2();
                return mkPartial (ast1, ast2, ast3);
            case zlt:
            case zeq:
            case zgt:
            case zeof:
            case zsemi:
            case zrparen:
            case zthen:
            case zelse:
            case zcomma:
                return mkNone ();
            default:
                return parseError (yarith2, new Tokens (zcomma, 
                        new Tokens (zelse, new Tokens (zeof, 
                        new Tokens (zeq, new Tokens (zgt, 
                        new Tokens (zlt, new Tokens (zminus, 
                        new Tokens (zplus, new Tokens (zrparen, 
                        new Tokens (zsemi, new Tokens (zthen, 
                        null))))))))))));
        }
    } /* end of parseArith2 */

    private Ast parseTerm () {
        Ast ast1, ast2;
        switch (nextToken()) {
            case zinteger:
            case zboolean:
            case zid:
            case zlparen:
                ast1 = parseFactor();
                ast2 = parseTerm2();
                return leftAssoc (ast1, ast2);
            default:
                return parseError (yterm, new Tokens (zboolean, 
                        new Tokens (zid, new Tokens (zinteger, 
                        new Tokens (zlparen, null)))));
        }
    } /* end of parseTerm */

    private Ast parseTerm2 () {
        Ast ast1, ast2, ast3;
        switch (nextToken()) {
            case ztimes:
                ast1 = parseMulop();
                ast2 = parseFactor();
                ast3 = parseTerm2();
                return mkPartial (ast1, ast2, ast3);
            case zplus:
            case zminus:
            case zcomma:
            case zelse:
            case zthen:
            case zrparen:
            case zsemi:
            case zeof:
            case zgt:
            case zeq:
            case zlt:
                return mkNone ();
            default:
                return parseError (yterm2, new Tokens (zcomma, 
                        new Tokens (zelse, new Tokens (zeof, 
                        new Tokens (zeq, new Tokens (zgt, 
                        new Tokens (zlt, new Tokens (zminus, 
                        new Tokens (zplus, new Tokens (zrparen, 
                        new Tokens (zsemi, new Tokens (zthen, 
                        new Tokens (ztimes, null)))))))))))));
        }
    } /* end of parseTerm2 */

    private Ast parseFactor () {
        Ast ast1, ast2;
        switch (nextToken()) {
            case zlparen:
                consumeToken();
                ast1 = parseFactor3();
                return identity (ast1);
            case zid:
                ast1 = parseId();
                ast2 = parseFactor2();
                return mkCall2 (ast1, ast2);
            case zboolean:
            case zinteger:
                ast1 = parseConst();
                return identity (ast1);
            default:
                return parseError (yfactor, new Tokens (zboolean, 
                        new Tokens (zid, new Tokens (zinteger, 
                        new Tokens (zlparen, null)))));
        }
    } /* end of parseFactor */

    private Ast parseFactor2 () {
        Ast ast1, ast2;
        switch (nextToken()) {
            case zlparen:
                consumeToken();
                ast1 = parseExprs();
                if (nextToken() == zrparen) {
                    consumeToken();
                    ast2 = parseFactor2();
                    return mkCalls (ast1, ast2);
                }
                else return parseError(yfactor2, new Tokens (zrparen, 
                        null));
            case ztimes:
            case zlt:
            case zeq:
            case zgt:
            case zeof:
            case zsemi:
            case zrparen:
            case zthen:
            case zelse:
            case zcomma:
            case zminus:
            case zplus:
                return mkNone ();
            default:
                return parseError (yfactor2, new Tokens (zcomma, 
                        new Tokens (zelse, new Tokens (zeof, 
                        new Tokens (zeq, new Tokens (zgt, 
                        new Tokens (zlparen, new Tokens (zlt, 
                        new Tokens (zminus, new Tokens (zplus, 
                        new Tokens (zrparen, new Tokens (zsemi, 
                        new Tokens (zthen, new Tokens (ztimes, 
                        null))))))))))))));
        }
    } /* end of parseFactor2 */

    private Ast parseFactor3 () {
        Ast ast1, ast2;
        switch (nextToken()) {
            case zif:
            case zlparen:
            case zid:
            case zboolean:
            case zinteger:
                ast1 = parseExpr();
                if (nextToken() == zrparen) {
                    consumeToken();
                    ast2 = parseFactor2();
                    return mkCall2 (ast1, ast2);
                }
                else return parseError(yfactor3, new Tokens (zrparen, 
                        null));
            case zlambda:
                ast1 = parseLambda2();
                ast2 = parseFactor2();
                return mkCall2 (ast1, ast2);
            default:
                return parseError (yfactor3, new Tokens (zboolean, 
                        new Tokens (zid, new Tokens (zif, 
                        new Tokens (zinteger, new Tokens (zlambda, 
                        new Tokens (zlparen, null)))))));
        }
    } /* end of parseFactor3 */

    private Ast parseRelop () {
        switch (nextToken()) {
            case zgt:
                consumeToken();
                return mkGT ();
            case zeq:
                consumeToken();
                return mkEQ ();
            case zlt:
                consumeToken();
                return mkLT ();
            default:
                return parseError (yrelop, new Tokens (zeq, 
                        new Tokens (zgt, new Tokens (zlt, null))));
        }
    } /* end of parseRelop */

    private Ast parseAddop () {
        switch (nextToken()) {
            case zminus:
                consumeToken();
                return mkMINUS ();
            case zplus:
                consumeToken();
                return mkPLUS ();
            default:
                return parseError (yaddop, new Tokens (zminus, 
                        new Tokens (zplus, null)));
        }
    } /* end of parseAddop */

    private Ast parseMulop () {
        switch (nextToken()) {
            case ztimes:
                consumeToken();
                return mkTIMES ();
            default:
                return parseError (ymulop, new Tokens (ztimes, null));
        }
    } /* end of parseMulop */

    private Ast parseFormals () {
        Ast ast1, ast2;
        switch (nextToken()) {
            case zid:
                ast1 = parseId();
                ast2 = parseFormals2();
                return cons (ast1, ast2);
            default:
                return parseError (yformals, new Tokens (zid, null));
        }
    } /* end of parseFormals */

    private Ast parseFormals2 () {
        Ast ast1;
        switch (nextToken()) {
            case zcomma:
                consumeToken();
                ast1 = parseFormals();
                return identity (ast1);
            case zrparen:
                return mkEmpty ();
            default:
                return parseError (yformals2, new Tokens (zcomma, 
                        new Tokens (zrparen, null)));
        }
    } /* end of parseFormals2 */

    private Ast parseExprs () {
        Ast ast1, ast2;
        switch (nextToken()) {
            case zif:
            case zlparen:
            case zid:
            case zboolean:
            case zinteger:
                ast1 = parseExpr();
                ast2 = parseExprs2();
                return cons (ast1, ast2);
            default:
                return parseError (yexprs, new Tokens (zboolean, 
                        new Tokens (zid, new Tokens (zif, 
                        new Tokens (zinteger, new Tokens (zlparen, 
                        null))))));
        }
    } /* end of parseExprs */

    private Ast parseExprs2 () {
        Ast ast1;
        switch (nextToken()) {
            case zcomma:
                consumeToken();
                ast1 = parseExprs();
                return identity (ast1);
            case zrparen:
                return mkEmpty ();
            default:
                return parseError (yexprs2, new Tokens (zcomma, 
                        new Tokens (zrparen, null)));
        }
    } /* end of parseExprs2 */

    private Ast parseId () {
        switch (nextToken()) {
            case zid:
                consumeToken();
                return mkId ();
            default:
                return parseError (yid, new Tokens (zid, null));
        }
    } /* end of parseId */

}
