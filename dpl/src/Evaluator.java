
public class Evaluator {

    private Environment e;

    public Evaluator(Environment e) {
        this.e = e;
    }

    public Lexeme eval(Lexeme tree, Lexeme env) {
        switch (tree.type) {
            case "INTEGER": return tree;
            case "STRING": return tree;
            case "VARIABLE": return e.lookupEnv(tree, env);
            case "OPAREN": return eval(tree.right, env);
            case "PLUS": return evalPlus(tree, env);
            case "MINUS": return evalMinus(tree, env);
            case "MULT": return evalMult(tree, env);
            case "DIVIDE": return evalDivide(tree, env);
            case "GREATER": return evalGreater(tree, env);
            case "AND": return evalAnd(tree, env);
            case "OR": return evalOr(tree, env);
            case "ASSIGN": return evalAssign(tree, env);
            case "LET": evalVarDef(tree, env);
            case "FUNCTION": return evalFuncDef(tree, env);
            case "IF": return evalIf(tree, env);
            case "WHILE": return evalWhile(tree, env);
            case "FUNC_CALL": return evalFuncCall(tree, env);
            case "BLOCK": return evalBlock(tree, env);
            default:
                System.out.println("BAD EXPRESSION!");
                System.exit(-1);
                return null;
        }
    }

    private Lexeme evalOr(Lexeme tree, Lexeme env) {
        return null;
    }

    private Lexeme evalAnd(Lexeme tree, Lexeme env) {
        return null;
    }

    private Lexeme evalGreater(Lexeme tree, Lexeme env) {
        return null;
    }

    private Lexeme evalDivide(Lexeme tree, Lexeme env) {
        return null;
    }

    private Lexeme evalMult(Lexeme tree, Lexeme env) {
        return null;
    }

    private Lexeme evalMinus(Lexeme tree, Lexeme env) {
        return null;
    }

    private Lexeme evalPlus(Lexeme tree, Lexeme env) {
        Lexeme left = eval(tree.left, env);
        Lexeme right = eval(tree.right, env);
        return new Lexeme("INTEGER", left.intVal + right.intVal);
    }

    private Lexeme evalAssign(Lexeme tree, Lexeme env) {
        Lexeme value = eval(tree.right, env);
        // update
        return value;
    }

    // Eval functions

    private Lexeme evalBlock(Lexeme tree, Lexeme env) {
        return null;
    }

    private Lexeme evalFuncCall(Lexeme tree, Lexeme env) {
        return null;
    }

    private Lexeme evalWhile(Lexeme tree, Lexeme env) {
        return null;
    }

    private Lexeme evalIf(Lexeme tree, Lexeme env) {
        return null;
    }

    private Lexeme evalFuncDef(Lexeme tree, Lexeme env) {
        return null;
    }

    private void evalVarDef(Lexeme tree, Lexeme env) {
        Lexeme val = eval(tree.right, env);
        e.insert(tree.left, val, env);
    }
}
