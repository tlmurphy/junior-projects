import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;

public class Scanner {

    public Scanner(String fileName) {
        try {
            BufferedReader br = new BufferedReader(new FileReader(fileName));
            new Lexer2(br);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
}
