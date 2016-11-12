import java.util.ArrayList;

/**
 * Lexeme Class
 */
public class Lexeme {

    private String strVal;
    int intVal;
    private boolean boolVal;
    private ArrayList arrayVal;
    private String varVal;

    String type;
    Lexeme left = null;
    Lexeme right = null;

    /**
     * Create a lexeme with just a type (keywords).
     * @param type Type of lexeme
     */
    public Lexeme(String type) {
        this.type = type;
    }

    /**
     * Create a Variable or String lexeme
     * @param type Type of lexeme
     * @param varOrStringVal Variable or String value
     */
    public Lexeme(String type, String varOrStringVal) {
        this.type = type;
        if (varOrStringVal.startsWith("\"")) this.strVal = varOrStringVal;
        else this.varVal = varOrStringVal;
    }

    /**
     * Create an Integer Number lexeme
     * @param type Type of lexeme
     * @param intVal Integer value
     */
    public Lexeme(String type, int intVal) {
        this.type = type;
        this.intVal = intVal;
    }

    /**
     * Create a Boolean Lexeme
     * @param type Type of Lexeme
     * @param boolVal Boolean Value (true or false)
     */
    public Lexeme(String type, boolean boolVal) {
        this.type = type;
        this.boolVal = boolVal;
    }

    /**
     * Create an Array Lexeme
     * @param type Type of Lexeme
     * @param arrayVal Array Value
     */
    public Lexeme(String type, ArrayList arrayVal) {
        this.type = type;
        this.arrayVal = arrayVal;
    }

    /**
     * Create an Environment Lexeme
     * @param type Type of Lexeme
     * @param left Car value
     * @param right Cdr Value
     */
    public Lexeme(String type, Lexeme left, Lexeme right) {
        this.type = type;
        this.left = left;
        this.right = right;
    }

    @Override
    public String toString() {
        String returnString = this.type + ": ";
        switch (this.type) {
            case "STRING":
                return returnString + this.strVal;
            case "INTEGER":
                return returnString + this.intVal;
            case "BOOLEAN":
                return returnString + this.boolVal;
            case "ARRAY":
                return returnString + this.arrayVal;
            case "VARIABLE":
                return returnString + this.varVal;
            default:
                return this.type.toUpperCase();
        }
    }
}

