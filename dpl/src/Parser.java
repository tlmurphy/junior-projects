import java.io.PushbackReader;
import java.io.FileNotFoundException;
import java.io.FileReader;

public class Parser {

    private Lexeme lexeme;
    private Lexer lexer;

    public Parser(String fileName) {
        PushbackReader br = null;
        try {
            br = new PushbackReader(new FileReader(fileName));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        lexer = new Lexer(br);
    }

    public Lexeme parse() {
        advance();
        Lexeme tree = statements();
        return tree;
    }

    private boolean varDefPending() {
        return check("LET");
    }

    private boolean printPending() {
        return check("PRINT");
    }

    public boolean statementPending() {
        return  expressionPending() ||
                varDefPending() ||
                printPending() ||
                check("COMMENT");
    }

    private Lexeme statement() {
        Lexeme tree = new Lexeme("STATEMENT");
        if (check("COMMENT")) {
            tree.left = match("COMMENT");
        }
        if (expressionPending()) {
            tree.left = expressTree();
        } else if (varDefPending()) {
            tree.left = varDefTree();
        } else if (printPending()) {
            tree.left = printLnTree();
        }
        return tree;
    }

    private Lexeme statements() {
        Lexeme tree = new Lexeme("STATEMENTS");
        if (statementPending()) {
            tree.left = statement();
            tree.right = statements();
        }

        return tree;
    }

    private Lexeme printLnTree() {
        Lexeme tree = match("PRINT");
        match("OPAREN");
        tree.right = expressTree();
        match("CPAREN");
        match("SEMI");
        return tree;
    }

    private boolean check(String type) {
        return lexeme.type.equals(type);
    }

    private Lexeme advance() {
        Lexeme prevLexeme = lexeme;
        lexeme = lexer.lex();
        return prevLexeme;
    }

    private Lexeme match(String type) {
        matchNoAdvance(type);
        return advance();
    }

    private void matchNoAdvance(String type) {
        if (!check(type)) {
            System.out.println("SYNTAX ERROR: Line " + lexer.lineNumber);
            System.exit(-1);
        }
    }

    private void unary() {
        if (check("INTEGER")) {
            match("INTEGER");
        } else if (check("STRING")) {
            match("STRING");
        } else if (check("BOOLEAN")) {
            match("BOOLEAN");
        } else { // Must be a variable
            match("VARIABLE");
        }
    }

    private Lexeme unaryTree() {
        Lexeme tree;
        if (check("INTEGER")) {
            tree = match("INTEGER");
        } else if (check("STRING")) {
            tree = match("STRING");
        } else if (check("BOOLEAN")) {
            tree = match("BOOLEAN");
        } else { // Must be a variable
            tree = match("VARIABLE");
        }
        return tree;
    }

    private boolean unaryPending() {
        return  check("INTEGER") ||
                check("STRING") ||
                check("VARIABLE");
    }

    private Lexeme operator() {
        return advance();
    }

    private boolean operatorPending() {
        return  check("PLUS") ||
                check("MINUS") ||
                check("MULT") ||
                check("DIVIDE") ||
                check("EQUAL") ||
                check("GREATER") ||
                check("GEQUAL") ||
                check("LESS") ||
                check("LEQUAL");
    }

    private boolean otherOperPending() {
        return  check("INC") ||
                check("DEC");
    }


    private void expression() {
        unary();
        if (operatorPending()) {
            operator();
            expression();
        } else if (otherOperPending()) {
            operator();
        }
    }

    private Lexeme expressTree() {
        Lexeme tree = unaryTree();
        if (operatorPending()) {
            Lexeme temp = operator();
            temp.left = tree;
            temp.right = expressTree();
            tree = temp;
        }
        return tree;
    }

    private boolean expressionPending() {
        return unaryPending();
    }

    private Lexeme functionDef() {
        if (check("VARIABLE")) {
            match("VARIABLE");
            // Match the parentheses and the optional parameter list
        }
        return null;
    }

    private boolean functionPending() {
        return check("FUNCTION");
    }

    private void varDef() {
        match("LET");
        match("VARIABLE");
        match("ASSIGN");
        if (functionPending()) {
            functionDef();
        } else { // expression
            expression();
        }
        match("SEMI");
    }

    private Lexeme varDefTree() {
        Lexeme tree = match("LET");
        tree.left = match("VARIABLE");
        match("ASSIGN");
        if (functionPending()) {
            tree.right = functionDef();
        } else {
            tree.right = expressTree();
        }
        match("SEMI");
        return tree;
    }
}
