package io.github.thanospapapetrou.funcky.runtime;

import java.util.function.Supplier;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyMonadicType;

public final class FunckyMonad extends FunckyValue {
    private static final String FORMAT = "Monad %1$s";

    private final FunckyMonadicType type;
    private final Supplier<FunckyExpression> base;

    public FunckyMonad(final FunckyEngine engine, final FunckyMonadicType type, final Supplier<FunckyExpression> base) {
        super(engine);
        this.type = type;
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
    public FunckyLiteral toExpression() {
        return new FunckyLiteral(engine, this);
    }

    @Override
    public int compareTo(final FunckyValue value) {
        if (value instanceof FunckyMonad monad) {
            return ((base == null) ? ((monad.base == null) ? 0 : -1) : ((monad.base == null) ? 1
                    : base.get().eval(engine.getContext()).compareTo(monad.base.get().eval(engine.getContext()))));
        }
        return super.compareTo(value);
    }

    @Override
    public int hashCode() {
        return ((base == null) ? 0 : base.get().eval(engine.getContext()).hashCode());
    }

    @Override
    public String toString() {
        base.get().eval(engine.getContext());
        return String.format(FORMAT, getType());
    }
}
