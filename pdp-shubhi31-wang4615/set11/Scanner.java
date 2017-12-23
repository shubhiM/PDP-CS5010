// Lexical analyzer for CS 5010 Problem Set 10, spring 2017.
//
//     new Scanner (s)
//
// where s is a String of tokens.

import java.util.List;
import java.util.ArrayList;

import java.util.Map;
import java.util.HashMap;

import java.io.IOException;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.charset.Charset;

public class Scanner {

    private static final int NONE = -1;          // no token is ready

    private static final char EOT = '\u0004';    // end of transmission (input)
    public  static final char LAMBDA = '\u03bb'; // lower-case Greek lambda

    private static final int START = 0;          // start state for DFA
    private static final int NUM   = 1;          // scanning a number
    private static final int ID    = 2;          // scanning an identifier
    private static final int COMMENT = 3;        // scanning a comment

    private String input;
    private int i;           // index of next character to be read
    private int start;       // index of character at start of current token

    private int line;        // line numbers for error messages
    private int linestart;   // index for start of the current line

    private int theToken = NONE;        // the current token, or NONE
    private String theTokenValue = "";  // the current token's value string
                                        //     (only for integer and id)

    public Scanner (String input) {
        this.input = input;
        this.i = 0;
        this.start = 0;
        this.line = 1;
        this.linestart = this.i;
        this.theToken = NONE;
    }

    // Returns the next (current) token.
    // Does not skip past the current token,
    // so consecutive calls to this method will return the same token.

    int nextToken () {
        if (theToken == NONE)
            dfa();
        return theToken;
    }

    // Forgets the current token.

    void consumeToken () {
        theToken = NONE;
    }

    // Returns the string of characters for the token.
    // (This is relevant for boolean, integer, and identifier tokens.)

    String tokenValue () {
        return theTokenValue;
    }

    // Given the nonterminal being parsed and a list of acceptable tokens,
    // reports the error and throws an exception.

    Ast parseError (int x, Tokens y) {
        String xstr = Tokens.terminalAsString (nextToken());
        System.err.print ("Syntax error encountered on line ");
        System.err.print (line + " (column " + (i - linestart) + ")");
        System.err.print (" while parsing ");
        System.err.println (Tokens.nonterminalAsString (x));
        System.err.print ("Encountered " + xstr);
        if (nextToken() == Parser.zid)
            System.err.println (" (" + tokenValue() + ")");
        System.err.println (" when only the following were legal:");
        Tokens.printTokens (y);
        throw new RuntimeException ("syntax error");
    }

    // Given an error message, reports the error and throws an exception.

    void scannerError (String msg) {
        System.err.println ("Syntax error encountered while scanning: ");
        System.err.println ("    " + msg);
        throw new RuntimeException ("syntax error");
    }

    // Deterministic finite automaton (state machine) for reading next token.

    void dfa () {
        int state = START;
        start = i;
        theToken = NONE;
        while (theToken == NONE) {
            char c = EOT;
            if (i < input.length()) {
                c = input.charAt (i);
            }
            switch (state) {

                // looking for start of token

                case START: 
                    switch (c) {
                        case EOT:    theToken = Parser.zeof;           break;
                        case ',':    theToken = Parser.zcomma;  i=i+1; break;
                        case '=':    theToken = Parser.zeq;     i=i+1; break;
                        case '>':    theToken = Parser.zgt;     i=i+1; break;
                        case LAMBDA: theToken = Parser.zlambda; i=i+1; break;
                        case '(':    theToken = Parser.zlparen; i=i+1; break;
                        case '<':    theToken = Parser.zlt;     i=i+1; break;
                        case '-':    theToken = Parser.zminus;  i=i+1; break;
                        case '+':    theToken = Parser.zplus;   i=i+1; break;
                        case ')':    theToken = Parser.zrparen; i=i+1; break;
                        case ';':    theToken = Parser.zsemi;   i=i+1; break;
                        case '*':    theToken = Parser.ztimes;  i=i+1; break;
                        case '#':    state = COMMENT; break;
                        default: {
                            if (c >= 128)
                                scannerError ("illegal character");
                            else if (Character.isWhitespace(c)) {
                                i = i + 1;
                                start = i;
                                if (c == '\n') {
                                    line = line + 1;
                                    linestart = i;
                                }
                            }
                            else if (Character.isDigit(c)) {
                                state = NUM;
                                i = i + 1;
                            }
                            else if (Character.isLetter(c)) {
                                state = ID;
                                i = i + 1;
                            }
                            else
                                scannerError ("illegal character");
                            break;
                        }
                    }
                    break;

                // parsing an integer

                case NUM: 
                    switch (c) {
                        default: {
                            if (Character.isDigit(c)) {
                                i = i + 1;
                            }
                            else {
                                theToken = Parser.zinteger;
                            }
                            break;
                        }
                    }
                    break;

                // parsing an identifier or keyword

                case ID:
                    switch (c) {
                        default: {
                            if (Character.isLetter(c) ||
                                Character.isDigit(c)) {
                                i = i + 1;
                            }
                            else {
                                theToken = Parser.zid;
                            }
                            break;
                        }
                    }
                    break;

                // skipping over a comment

                case COMMENT:
                    switch (c) {
                        default: {
                            i = i + 1;
                            if (c == '\n') {
                                line = line + 1;
                                linestart = i;
                                state = START;
                            }
                            break;
                        }
                    }
                    break;
            }
        }
        if (theToken == Parser.zinteger)
            theTokenValue = input.substring (start, i);
        else if (theToken == Parser.zid) {
            theTokenValue = input.substring (start, i);
            if (theTokenValue.equals ("true"))
                theToken = Parser.zboolean;
            else if (theTokenValue.equals ("false"))
                theToken = Parser.zboolean;
            else if (theTokenValue.equals ("else"))
                theToken = Parser.zelse;
            else if (theTokenValue.equals ("if"))
                theToken = Parser.zif;
            else if (theTokenValue.equals ("then"))
                theToken = Parser.zthen;
        }
    }

    // Reads a program from the given file and returns it as a string.

    public static String readPgm (String filename) {
        StringBuffer pgm = new StringBuffer();
        List<ExpVal> inputs = new ArrayList<ExpVal>();
        List<String> lines = null;
        try {
            lines = Files.readAllLines (Paths.get (filename),
                                        Charset.forName ("UTF-8"));
        }
        catch (IOException e) {
            System.out.print ("IO Exception reading from file: ");
            System.out.println (filename);
            System.out.println (e);
            return "";
        }
        for (String line : lines) {
            pgm.append (line);
            pgm.append ("\n");
        }
        return pgm.toString();
    }

    // Parses the given program and returns it as a List<Def>.

    public static List<Def> parsePgm (String pgm0) {
        Scanner scnr = new Scanner (pgm0);
        Actions actions = new Actions (scnr);
        Parser parser = new Parser (scnr, actions);
        List<Def> pgm = null;
        try {
            Ast defs = parser.parsePgm();
            pgm = defs.asPgm();
        }
        catch (Exception e) {
	    System.err.println ("Exception thrown during parsing: ");
	    e.printStackTrace();
            // return a program that should throw an exception
            // when interpreted
	    pgm.add (Asts.def ("SYNTAXERROR",
			       Asts.constantExp (Asts.expVal (false))));
        }
        return pgm;
    }

    // Parses the given expression and returns it as an Exp.

    public static Exp parseExp (String exp) {
        List<Def> pgm = parsePgm ("f (x) " + exp);
        return pgm.get(0).rhs().asLambda().body();
    }

    // Parses and runs the given program on the given inputs.

    public static ExpVal runPgm (String pgm0, List<ExpVal> inputs) {
        List<Def> pgm = parsePgm (pgm0);
        ExpVal result = Asts.expVal (false);
        try {
            result = Programs.run (pgm, inputs);
        }
        catch (Exception e) {
            System.err.println (e);
        }
        return result;
    }

    // Runs the ps11 program named on the command line on the
    // integer inputs that follow it on the command line,
    // printing the result computed by the program.

    public static void main (String[] args) {
        if (args.length >= 2) {
            String filename = args[0];
            String pgm = readPgm (filename);
            List<ExpVal> inputs = new ArrayList<ExpVal>();
            for (int i = 1; i < args.length; i = i + 1) {
                long input = Long.parseLong (args[i]);
                inputs.add (Asts.expVal (input));
            }
            ExpVal result = runPgm (pgm, inputs);
            System.out.println (result);
        }
        else
            System.out.println (usageMsg);
    }

    public static final String usageMsg
        = "Usage: java Programs <filename> <input> ...";
}

