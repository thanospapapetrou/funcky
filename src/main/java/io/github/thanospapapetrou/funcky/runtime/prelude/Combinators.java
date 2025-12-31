package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.util.List;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType.FUNCTION;

public final class Combinators extends FunckyLibrary {
    private final FunckyTypeVariable a = new FunckyTypeVariable(context);
    private final FunckyTypeVariable b = new FunckyTypeVariable(context);
    private final FunckyTypeVariable c = new FunckyTypeVariable(context);
    public final HigherOrderFunction s =
            new HigherOrderFunction(context, FUNCTION(context -> a, context -> b, context -> c),
                    FUNCTION(context -> a, context -> b), context -> a, context -> c) {
        @Override
        public FunckyValue apply(final List<FunckyExpression> arguments, final FunckyContext context) {
            return new FunckyApplication(new FunckyApplication(arguments.get(0), arguments.get(2)),
                    new FunckyApplication(arguments.get(1), arguments.get(2))).eval(context);
        }
    };
    public final HigherOrderFunction k = new HigherOrderFunction(context, context -> a, context -> b, context -> a) {
        @Override
        public FunckyValue apply(final List<FunckyExpression> arguments, final FunckyContext context) {
            return arguments.getFirst().eval(context);
        }
    };

    public Combinators(final FunckyContext context) {
        super(context);
    }
}
