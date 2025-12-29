package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.FunckyFunction;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType.FUNCTION;

public abstract class HigherOrderFunction extends FunckyFunction {
    private final int order;
    private final FunckyExpression expression;
    private final List<FunckyExpression> arguments;

    public HigherOrderFunction(final FunckyEngine engine, final FunckyFunctionType type, final int order,
            final FunckyExpression expression) {
        this(engine, type, order, expression, List.of());
    }

    HigherOrderFunction(final FunckyEngine engine, final Function<FunckyEngine, ? extends FunckyType>... types) {
        this(engine, FUNCTION(types).apply(engine), types.length - 1, null);
    }

    private HigherOrderFunction(final FunckyEngine engine, final FunckyFunctionType type, final int order,
            final FunckyExpression expression, final List<FunckyExpression> arguments) {
        super(engine, type);
        this.order = order;
        this.expression = expression;
        this.arguments = arguments;
    }

    public int getOrder() {
        return order;
    }

    @Override
    public FunckyValue apply(final FunckyExpression argument, final ScriptContext context) {
            final HigherOrderFunction that = this;
            final FunckyType range = (FunckyType) ((FunckyFunctionType) that.type.unify(
                    new FunckyFunctionType(engine, new FunckyLiteral(engine, argument.getType()), new FunckyLiteral(engine, new FunckyTypeVariable(engine))))).getRange()
                    .eval(engine.getContext());
            final List<FunckyExpression> arguments = new ArrayList<>(this.arguments);
            arguments.add(argument);
        return (order > 1) ? new HigherOrderFunction(engine, (FunckyFunctionType) range, order - 1,
                new FunckyApplication(that.toExpression(), argument), arguments) {
                @Override
                public FunckyValue apply(final List<FunckyExpression> arguments, final ScriptContext context) {
                    return that.apply(arguments, context);
                }
        } : apply(arguments, context);
    }

    public abstract FunckyValue apply(final List<FunckyExpression> arguments, final ScriptContext context);

    @Override
    public FunckyExpression toExpression() {
        return expression;
    }
}
