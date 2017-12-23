// Lists of tokens, used only for syntax error messages.
// Each token is represented as an int.
// The list is terminated by null.

class Tokens {

    private int first;      // the first token in this list
    private Tokens rest;    // the rest of this list

    Tokens (int first, Tokens rest) {
        this.first = first;
        this.rest = rest;
    }

    static void printTokens (Tokens tokens) {
        while (tokens != null) {
            System.err.println (terminalAsString (tokens.first));
            tokens = tokens.rest;
        }
    }

    // Returns the given terminal as a readable string.

    static String terminalAsString (int k) {
        switch (k) {
            case Parser.zboolean:  return "boolean";
            case Parser.zcomma:    return ",";
            case Parser.zelse:     return "else";
            case Parser.zeof:      return "end-of-input";
            case Parser.zeq:       return "=";
            case Parser.zgt:       return ">";
            case Parser.zid:       return "identifier";
            case Parser.zif:       return "if";
            case Parser.zinteger:  return "integer";
            case Parser.zlambda:   return "lambda";
            case Parser.zlparen:   return "(";
            case Parser.zlt:       return "<";
            case Parser.zminus:    return "-";
            case Parser.zplus:     return "+";
            case Parser.zrparen:   return ")";
            case Parser.zsemi:     return ";";
            case Parser.zthen:     return "then";
            case Parser.ztimes:    return "*";
            default:               return "PARSER BUG";
        }

    }

    // Returns the given nonterminal as a readable string.

    static String nonterminalAsString (int k) {
        switch (k) {
            case Parser.yaddop:    return "<addop>";
            case Parser.yarith:    return "<arith>";
            case Parser.yarith2:   return "<arith>";
            case Parser.yconst:    return "<const>";
            case Parser.ydefn:     return "<defn>";
            case Parser.ydefn2:    return "<defn>";
            case Parser.yexpr:     return "<expr>";
            case Parser.yexpr2:    return "<expr>";
            case Parser.yexprs:    return "<exprs>";
            case Parser.yexprs2:   return "<exprs>";
            case Parser.yfactor:   return "<factor>";
            case Parser.yfactor2:  return "<factor>";
            case Parser.yfactor3:  return "<factor>";
            case Parser.yformals:  return "<formals>";
            case Parser.yformals2: return "<formals>";
            case Parser.yid:       return "<identifier>";
            case Parser.yif:       return "<if>";
            case Parser.ylambda:   return "<lambda>";
            case Parser.ylambda2:  return "<lambda>";
            case Parser.ymulop:    return "<mulop>";
            case Parser.ypgm:      return "<program>";
            case Parser.ypgm2:     return "<program>";
            case Parser.yrelop:    return "<relop>";
            case Parser.yterm:     return "<term>";
            case Parser.yterm2:    return "<term>";
            default:               return "PARSER BUG";
        }
    }
}