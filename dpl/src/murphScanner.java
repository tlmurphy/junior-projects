import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;

public class murphScanner {

    public murphScanner(String fileName) {
        BufferedReader br = null;
        try {
            br = new BufferedReader(new FileReader(fileName));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        Lexer lexer = new Lexer(br);
        Lexeme lexeme = lexer.lex();

        while (!lexeme.type.equals("END")) {
            System.out.println(lexeme.toString());
            lexeme = lexer.lex();
        }

        System.out.println(lexeme.toString());
    }
}
