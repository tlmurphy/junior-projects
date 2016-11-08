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

    public void parse() {
        advance();
        while (!check("END")) {
            if (check("LET")) {
                varDef();
            }
            advance();
        }
    }

    private boolean check(String type) {
        return lexeme.type.equals(type);
    }

    private Lexeme advance() {
        lexeme = lexer.lex();
        System.out.println(lexeme.toString());
        return lexeme;
    }

    private Lexeme match(String type) {
        matchNoAdvance(type);
        return advance();
    }

    private void matchNoAdvance(String type) {
        if (!check(type)) {
            System.out.println("SYNTAX ERROR!");
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

    private boolean unaryPending() {
        return  check("INTEGER") ||
                check("STRING") ||
                check("VARIABLE");
    }

    private void operator() {
        advance();
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

    private boolean expressionPending() {
        return unaryPending();
    }

    private void functionDef() {
        if (check("VARIABLE")) {
            match("VARIABLE");
            // Match the parentheses and the optional parameter list
        }
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

    private Lexeme unaryTree() {
        Lexeme tree;

        if (check("VARIABLE")) tree = advance();
        else if (check("INTEGER")) tree = advance();
//        else if (check("OPAREN")) {
//            tree = match("OPAREN");
//            tree.left = null;
//            tree.right = expression();
//        }
        else {
            match("MINUS");
            tree = new Lexeme("UMINUS");
            tree.left = null;
            tree.right = unaryTree();
        }
        return tree;
    }
}
