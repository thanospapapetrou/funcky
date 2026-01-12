package io.github.thanospapapetrou.funcky.runtime;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType;

public non-sealed abstract class FunckyFunction extends FunckyValue {
    protected final FunckyFunctionType type;

    protected FunckyFunction(final FunckyFunctionType type) {
        this(type.getContext(), type);
    }

    private FunckyFunction(final FunckyContext context, final FunckyFunctionType type) {
        super(context);
        this.type = type;
    }

    public abstract FunckyValue apply(final FunckyExpression argument, final FunckyContext context);

    @Override
    public FunckyFunctionType getType() {
        return type;
    }

    @Override
    public int compareTo(final FunckyValue value) {
        return (value instanceof FunckyFunction function) ? toString().compareTo(function.toString())
                : super.compareTo(value);
    }

    @Override
    public int hashCode() {
        return toString().hashCode();
    }
}
