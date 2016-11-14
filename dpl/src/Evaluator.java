
public class Evaluator {

    private Environment e;

    public Evaluator(Environment e) {
        this.e = e;
    }

    public Lexeme eval(Lexeme tree, Lexeme env) {
        if (tree == null) {
            return null;
        }
        switch (tree.type) {
            case "STATEMENTS":
                eval(tree.left, env);
                return eval(tree.right, env);
            case "STATEMENT": return eval(tree.left, env);
            case "COMMENT": return null;
            case "INTEGER": return tree;
            case "STRING": return tree;
            case "VARIABLE": return e.lookupEnv(tree, env);
            case "OPAREN": return eval(tree.right, env);
            case "PLUS":
            case "MINUS":
            case "MULT":
            case "DIVIDE":
            case "GREATER":
            case "LESS":
            case "GEQUAL":
            case "LEQUAL":
            case "EQUAL": return evalOperator(tree, env);
            case "AND": return evalAnd(tree, env);
            case "OR": return evalOr(tree, env);
            case "ASSIGN": return evalVarAssign(tree, env);
            case "LET": return evalVarDef(tree, env);
            case "FUNCTION": return evalFuncDef(tree, env);
            case "IF": return evalIf(tree, env);
            case "WHILE": return evalWhile(tree, env);
            case "FUNC_CALL": return evalFuncCall(tree, env);
            case "PRINT": return evalPrint(tree.right, env);
            default:
                System.out.println("BAD EXPRESSION!");
                System.exit(-1);
        }
        return null;
    }

    private Lexeme evalFuncCall(Lexeme tree, Lexeme env) {
//        String name = getCallName(tree);
//        Lexeme args = getFuncCallArgs(tree);
//        Lexeme eargs = evalArgs(args,env);
//        // Checking for built in functions
//        switch (name) {
//            case "println": return evalPrintln(eargs);
//        }
//        Lexeme params = getClosureParams(closure);
//        Lexeme body = getClosureBody(closure);
//        Lexeme senv = getClosureEnvironment(closure);
//        Lexeme xenv = EnvExtend(senv,params,eargs);
//
//        return eval(body,xenv);
        return null;
    }

    private Lexeme evalPrint(Lexeme tree, Lexeme env) {
        System.out.println(eval(tree, env));
        return null;
    }

    private Lexeme evalOperator(Lexeme tree, Lexeme env) {
        Lexeme left = eval(tree.left, env);
        Lexeme right = eval(tree.right, env);
        switch (tree.type) {
            case "PLUS": return new Lexeme("INTEGER", left.intVal + right.intVal);
            case "MINUS": return new Lexeme("INTEGER", left.intVal - right.intVal);
            case "MULT": return new Lexeme("INTEGER", left.intVal * right.intVal);
            case "DIVIDE": return new Lexeme("INTEGER", left.intVal / right.intVal);
            case "GEQUAL": return new Lexeme("BOOLEAN", left.intVal >= right.intVal);
            case "GREATER": return new Lexeme("BOOLEAN", left.intVal > right.intVal);
            case "LESS": return new Lexeme("BOOLEAN", left.intVal < right.intVal);
            case "LEQUAL": return new Lexeme("BOOLEAN", left.intVal <= right.intVal);
            case "EQUAL": return new Lexeme("BOOLEAN", left.intVal == right.intVal);
            default:
                System.out.println("Defaulted on the evalOperator function for some reason...");
                System.exit(-1);
                return null;
        }
    }

    private Lexeme evalAnd(Lexeme tree, Lexeme env) {
        return null;
    }

    private Lexeme evalOr(Lexeme tree, Lexeme env) {
        return null;
    }

    private Lexeme evalVarAssign(Lexeme tree, Lexeme env) {
        Lexeme var = tree.left;
        Lexeme val = tree.right;
        e.updateEnv(var, val, env);
        return null;
    }

    private Lexeme evalWhile(Lexeme tree, Lexeme env) {
        return null;
    }

    private Lexeme evalIf(Lexeme tree, Lexeme env) {
        Lexeme ifExpression = tree.left.left;
        Lexeme ifBody = tree.left.right;
        Lexeme elseStatement = tree.right;
        Lexeme local = e.extendEnv(env, e.getVars(env), e.getVals(env));
        if (eval(ifExpression, env).boolVal) {
            eval(ifBody, local);
        } else {
            eval(elseStatement, local);
        }
        return null;
    }

    private Lexeme evalFuncDef(Lexeme tree, Lexeme env) {
        return null;
    }

    private Lexeme evalVarDef(Lexeme tree, Lexeme env) {
        Lexeme val = eval(tree.right, env);
        e.insert(tree.left, val, env);
        return null;
    }
}
