package io.github.thanospapapetrou.funcky.runtime;

import java.util.function.Supplier;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyMonadicType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;

public final class FunckyMonad extends FunckyValue {
    private static final String FORMAT = "monad_%1$x";

    private final FunckyMonadicType type;
    private final Supplier<FunckyExpression> base;

    public FunckyMonad(final FunckyContext context, final Object type, final Supplier<FunckyExpression> base) {
        super(context);
        this.type = (FunckyMonadicType) FunckyType.type(type).apply(context);
        this.base = base;
    }

    public FunckyExpression getBase() {
        return base.get();
    }

    @Override
    public FunckyMonadicType getType() {
        return type;
    }

    @Override
    public int compareTo(final FunckyValue value) {
        if (value instanceof FunckyMonad monad) {
            final int comparison = type.compareTo(monad.type);
            return (comparison == 0) ? base.get().eval(context).compareTo(monad.base.get().eval(context)) : comparison;
        }
        return super.compareTo(value);
    }

    @Override
    public int hashCode() {
        return type.hashCode() + base.get().eval(context).hashCode();
    }

    @Override
    public String toString() {
        return String.format(FORMAT, hashCode());
    }
}
