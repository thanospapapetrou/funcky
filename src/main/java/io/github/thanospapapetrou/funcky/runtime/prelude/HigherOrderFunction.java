package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.runtime.FunckyFunction;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType.FUNCTION;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable.VAR;

public abstract class HigherOrderFunction extends FunckyFunction {
    private final int order;
    private final FunckyExpression expression;
    private final List<FunckyExpression> arguments;

    public HigherOrderFunction(final FunckyContext context, final FunckyFunctionType type, final int order,
            final FunckyExpression expression) {
        this(context, type, order, expression, List.of());
    }

    HigherOrderFunction(final FunckyContext context, final Function<FunckyContext, ? extends FunckyType>... types) {
        this(context, FUNCTION(types).apply(context), types.length - 1, null); // TODO replace functions with objects
    }

    private HigherOrderFunction(final FunckyContext context, final FunckyFunctionType type, final int order,
            final FunckyExpression expression, final List<FunckyExpression> arguments) {
        super(context, type);
        this.order = order;
        this.expression = expression;
        this.arguments = arguments;
    }

    public int getOrder() {
        return order;
    }

    @Override
    public FunckyValue apply(final FunckyExpression argument, final FunckyContext context) {
        final HigherOrderFunction that = this;
        final FunckyType range = (FunckyType) ((FunckyFunctionType) that.type.unify(FUNCTION(argument.getType(), VAR)
                .apply(context))).getRange().eval(this.context);
        final List<FunckyExpression> arguments = new ArrayList<>(this.arguments);
        arguments.add(argument);
        return (order > 1) ? new HigherOrderFunction(this.context, (FunckyFunctionType) range, order - 1,
                new FunckyApplication(that.toExpression(), argument), arguments) {
                @Override
                public FunckyValue apply(final List<FunckyExpression> arguments, final FunckyContext context) {
                    return that.apply(arguments, context);
                }
        } : apply(arguments, context);
    }

    public abstract FunckyValue apply(final List<FunckyExpression> arguments, final FunckyContext context);

    @Override
    public FunckyExpression toExpression() {
        return expression;
    }
}
