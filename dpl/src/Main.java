public class Main {

    public static void main(String[] args) {
        Parser p = new Parser(args[0]);
        Lexeme parseTree = p.parse();
        Environment env = new Environment();
        Evaluator e = new Evaluator(env);
        Lexeme global = env.createEnv();
        Lexeme iter = parseTree;
        while (iter.right != null) {
            e.eval(iter, global);
            iter = iter.right;
        }
    }
}
