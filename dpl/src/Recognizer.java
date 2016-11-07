public class Recognizer {

    private Lexeme currentLexeme;
    private boolean check(String type) {
        return currentLexeme.type.equals(type);
    }
}
