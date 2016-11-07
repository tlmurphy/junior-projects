import java.io.BufferedReader;
import java.io.IOException;

public class Lexer {

    private BufferedReader reader;

    public Lexer(BufferedReader reader) {
        this.reader = reader;
    }

    public Lexeme lex() {
        char ch;
        try {

            int r = reader.read();
            r = skipWhiteSpace(r);
            if (r == -1) return new Lexeme("END");

            ch = (char) r;
            switch (ch) {
                case '(':
                    return new Lexeme("OPAREN");
                case ')':
                    return new Lexeme("CPAREN");
                case ',':
                    return new Lexeme("COMMA");
                case '+':
                    return new Lexeme("PLUS");
                case '*':
                    return new Lexeme("MULT");
                case '-':
                    return new Lexeme("MINUS");
                case '/':
                    return new Lexeme("DIVIDES");
                case '<':
                    return new Lexeme("LESS");
                case '>':
                    return new Lexeme("GREATER");
                case '{':
                    return new Lexeme("OBRACK");
                case '}':
                    return new Lexeme("CBRACK");
                case '=':
                    return new Lexeme("ASSIGN");
                case ';':
                    return new Lexeme("SEMI");
                default:
                    if (Character.isDigit(ch)) {
                        return lexNumber(readMoreNum(ch));
                    } else if (Character.isAlphabetic(ch)) {
                        return lexKeyword(readMoreKeyword(ch));
                    } else if (ch == '\"') {
                        return new Lexeme("STRING", readMoreString(ch));
                    }
            }
        }
        catch (IOException e) { e.printStackTrace(); }
        return new Lexeme("BAD_CHARACTER");
    }

    private int skipWhiteSpace(int r) throws IOException {
        if (r == 32 || r == 10) {
            return skipWhiteSpace(reader.read());
        } else return r;
    }

    private String readMoreNum(char ch) throws IOException {
        String numString = "";
        while (Character.isDigit(ch) || ch == '.') {
            numString += ch;
            ch = (char) reader.read();
        }
        return numString;
    }

    private String readMoreKeyword(char ch) throws IOException {
        String keyString = "";
        while (Character.isAlphabetic(ch)) {
            keyString += ch;
            ch = (char) reader.read();
        }
        return keyString;
    }

    private String readMoreString(char ch) throws IOException {
        String newString = "";
        newString += ch;
        ch = (char) reader.read();

        while (ch != '\"') {
            newString += ch;
            ch = (char) reader.read();
        }

        newString += ch; // Needed to add the ending quote on
        return newString;
    }

    private Lexeme lexNumber(String num) {
        if (num.indexOf('.') >= 0) return new Lexeme("REAL", Double.parseDouble(num));
        else return new Lexeme("INTEGER", Integer.parseInt(num));
    }

    private Lexeme lexKeyword(String key) {
        switch (key) {
            case "let":
                return new Lexeme("LET");
            case "func":
                return new Lexeme("FUNCTION");
            case "if":
                return new Lexeme("IF");
            case "else":
                return new Lexeme("ELSE");
            case "print":
                return new Lexeme("PRINT");
            case "return":
                return new Lexeme("RETURN");
            default:
                return new Lexeme("VARIABLE", key);
        }
    }
}
