package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.util.ArrayList;
import java.util.List;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.compiler.CompilationException;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.runtime.FunckyFunction;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;

public abstract class HigherOrderFunction extends FunckyFunction {
    private final int order;
    private final FunckyExpression expression;
    private final List<FunckyExpression> arguments;

    HigherOrderFunction(final Class<? extends FunckyLibrary> library, final String name, final FunckyType... types) {
        this(new FunckyFunctionType(types), types.length - 1, new FunckyReference(library, name), List.of());
    }

    private HigherOrderFunction(final FunckyFunctionType type, final int order, final FunckyExpression expression,
            final List<FunckyExpression> arguments) {
        super(type);
        this.order = order;
        this.expression = expression;
        this.arguments = arguments;
    }

    @Override
    public FunckyValue apply(final FunckyExpression argument, final ScriptContext context)
            throws FunckyRuntimeException {
        try {
            final HigherOrderFunction that = this;
            final FunckyType range = (FunckyType) ((FunckyFunctionType) that.type.unify(
                    new FunckyFunctionType(argument.getType(), new FunckyTypeVariable()))).getRange().eval();
            final List<FunckyExpression> arguments = new ArrayList<>(this.arguments);
            arguments.add(argument);
            return (order > 1) ? new HigherOrderFunction((FunckyFunctionType) range, order - 1,
                    new FunckyApplication(that.toExpression(), argument), arguments) {
                @Override
                protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments)
                        throws FunckyRuntimeException {
                    return that.apply(context, arguments);
                }
            } : apply(context, arguments);
        } catch (final CompilationException e) {
            throw new FunckyRuntimeException(e);
        }
    }

    protected abstract FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments)
            throws FunckyRuntimeException;

    @Override
    public FunckyExpression toExpression() {
        return expression;
    }
}
