import java.lang.reflect.Type;

/**
 * Lexeme Class
 */
public class Lexeme implements Type {

    private String type;
    private String string;
    private int integer;
    private double real;

    /**
     * Create a lexeme with just a type.
     * @param type Type of lexeme
     */
    public Lexeme(String type) {
        this.type = type;
    }

    /**
     * Create a String lexeme
     * @param type Type of lexeme
     * @param string String value
     */
    public Lexeme(String type, String string) {
        this.type = type;
        this.string = string;
    }

    /**
     * Create an Integer Number lexeme
     * @param type Type of lexeme
     * @param integer Integer value
     */
    public Lexeme(String type, int integer) {
        this.type = type;
        this.integer = integer;
    }

    /**
     * Create a Real Number Lexeme
     * @param type Type of lexeme
     * @param real Real value
     */
    public Lexeme(String type, double real) {
        this.type = type;
        this.real = real;
    }
}

