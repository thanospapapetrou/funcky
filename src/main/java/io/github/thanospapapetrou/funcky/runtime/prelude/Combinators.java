package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.util.List;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;

import static io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType.FUNCTION;

public non-sealed class Combinators extends FunckyLibrary {
    private final FunckyTypeVariable $_a = new FunckyTypeVariable(engine);
    private final FunckyTypeVariable $_b = new FunckyTypeVariable(engine);
    private final FunckyTypeVariable $_c = new FunckyTypeVariable(engine);
    public final HigherOrderFunction $s = new HigherOrderFunction(engine, this,
            FUNCTION(engine -> $_a, engine -> $_b, engine -> $_c), FUNCTION(engine -> $_a, engine -> $_b), engine -> $_a, engine -> $_c) {
        @Override
        protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyApplication(new FunckyApplication(arguments.get(0), arguments.get(2)),
                    new FunckyApplication(arguments.get(1), arguments.get(2))).eval(context);
        }
    };
    public final HigherOrderFunction $k = new HigherOrderFunction(engine, this, engine -> $_a, engine -> $_b, engine -> $_a) {
        @Override
        protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return arguments.getFirst().eval(context);
        }
    };

    public Combinators(final FunckyEngine engine) {
        super(engine);
    }
}
