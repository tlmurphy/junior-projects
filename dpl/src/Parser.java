import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;

public class Parser {

    private Lexeme lexeme;
    private Lexer lexer;

    public Parser(String fileName) {
        BufferedReader br = null;
        try {
            br = new BufferedReader(new FileReader(fileName));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        lexer = new Lexer(br);
    }

    public void parse() {
        advance();
        while (!check("END")) {
            System.out.println(lexeme.toString());
            advance();
        }
    }

    private boolean check(String type) {
        return lexeme.type.equals(type);
    }

    private Lexeme advance() {
        lexeme = lexer.lex();
        return lexeme;
    }

    private Lexeme match(String type) {
        matchNoAdvance(type);
        return advance();
    }

    private void matchNoAdvance(String type) {
        if (!check(type)) {
            System.err.println("SYNTAX ERROR!");
        }
    }


    // Example parse
    private void expression() {
        unary();
        if (operatorPending()) {
            //operator();
            expression();
        }
    }

    private boolean operatorPending() {
        return check("PLUS") || check("TIMES");
    }

    private void unary() {
        if (check("INTEGER")) {
            match("INTEGER");
        } else if (check("REAL")) {
            match("REAL");
        } else if (check("VARIABLE")) {
            match("VARIABLE");
            if (check("OPAREN")) {
                match("OPAREN");
                // optExpressionList();
                match("CPAREN");
            }
        } else {
            match("OPAREN");
            expression();
            match("CPAREN");
        }
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
