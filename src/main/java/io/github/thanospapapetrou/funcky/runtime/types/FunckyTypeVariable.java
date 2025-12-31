package io.github.thanospapapetrou.funcky.runtime.types;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;

public final class FunckyTypeVariable extends FunckyType {
    private static final AtomicInteger HASH = new AtomicInteger();
    private static final String FORMAT = "$_%1$x";

    private final int hash;

    public FunckyTypeVariable(final FunckyContext context) {
        this(context, HASH.getAndIncrement());
    }

    private FunckyTypeVariable(final FunckyContext context, final int hash) {
        super(context);
        this.hash = hash;
    }

    @Override
    public FunckyLiteral toExpression() {
        return new FunckyLiteral(context.getEngine(), this);
    }

    @Override
    public int compareTo(final FunckyValue value) {
        return (value instanceof FunckyTypeVariable variable) ? Integer.compare(hash, variable.hash)
                : super.compareTo(value);
    }

    @Override
    public int hashCode() {
        return hash;
    }

    @Override
    public String toString() {
        return String.format(FORMAT, hash);
    }

    @Override
    protected Set<FunckyTypeVariable> getTypeVariables() {
        return Set.of(this);
    }

    @Override
    protected FunckyType bind(final Map<FunckyTypeVariable, FunckyType> bindings) {
        return bindings.getOrDefault(this, this);
    }
}
