package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.util.List;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

public class Combinators extends FunckyLibrary {
    private static final FunckyTypeVariable A = new FunckyTypeVariable();
    private static final FunckyTypeVariable B = new FunckyTypeVariable();
    private static final FunckyTypeVariable C = new FunckyTypeVariable();
    public static final HigherOrderFunction S = new HigherOrderFunction(Combinators.class, "s",
            new FunckyFunctionType(A, B, C), new FunckyFunctionType(A, B), A, C) {
        @Override
        protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments)
                throws FunckyRuntimeException {
            return new FunckyApplication(new FunckyApplication(arguments.get(0), arguments.get(2)),
                    new FunckyApplication(arguments.get(1), arguments.get(2))).eval(context);
        }
    };
    public static final HigherOrderFunction K = new HigherOrderFunction(Combinators.class, "k",
            A, B, A) {
        @Override
        protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments)
                throws FunckyRuntimeException {
            return arguments.get(0).eval(context);
        }
    };

    public Combinators() {
        super(S, K);
    }
}
