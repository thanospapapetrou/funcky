package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.util.List;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType.FUNCTION;

public final class Combinators extends FunckyLibrary {
    private final FunckyTypeVariable a = new FunckyTypeVariable(engine);
    private final FunckyTypeVariable b = new FunckyTypeVariable(engine);
    private final FunckyTypeVariable c = new FunckyTypeVariable(engine);
    public final HigherOrderFunction s =
            new HigherOrderFunction(engine,
                    FUNCTION(engine -> a, engine -> b, engine -> c),
                    FUNCTION(engine -> a, engine -> b), engine -> a, engine -> c) {
        @Override
        public FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyApplication(new FunckyApplication(arguments.get(0), arguments.get(2)),
                    new FunckyApplication(arguments.get(1), arguments.get(2))).eval(context);
        }
    };
    public final HigherOrderFunction k = new HigherOrderFunction(engine, engine -> a, engine -> b, engine -> a) {
        @Override
        public FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return arguments.getFirst().eval(context);
        }
    };

    public Combinators(final FunckyEngine engine) {
        super(engine);
    }
}
