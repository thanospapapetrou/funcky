package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.util.List;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyExpression;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;

import static io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType.FUNCTION;

public non-sealed class Combinators extends FunckyLibrary {
    private final FunckyTypeVariable $_a = new FunckyTypeVariable();
    private final FunckyTypeVariable $_b = new FunckyTypeVariable();
    private final FunckyTypeVariable $_c = new FunckyTypeVariable();
    public final HigherOrderFunction $s = new HigherOrderFunction(this,
            FUNCTION($_a, $_b, $_c), FUNCTION($_a, $_b), $_a, $_c) {
        @Override
        protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyApplication(new FunckyApplication(arguments.get(0), arguments.get(2)),
                    new FunckyApplication(arguments.get(1), arguments.get(2))).eval(context);
        }
    };
    public final HigherOrderFunction $k = new HigherOrderFunction(this, $_a, $_b, $_a) {
        @Override
        protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return arguments.getFirst().eval(context);
        }
    };

    public Combinators(final FunckyEngine engine) {
        super(engine);
    }
}
