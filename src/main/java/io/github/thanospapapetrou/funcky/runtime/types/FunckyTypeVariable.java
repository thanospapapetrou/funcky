package io.github.thanospapapetrou.funcky.runtime.types;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;

public final class FunckyTypeVariable extends FunckyType {
    private static final AtomicInteger HASH = new AtomicInteger();
    private static final String FORMAT = "$_%1$x";

    private final int hash;

    public FunckyTypeVariable(final FunckyEngine engine) {
        this(engine, HASH.getAndIncrement());
    }

    private FunckyTypeVariable(final FunckyEngine engine, final int hash) {
        super(engine);
        this.hash = hash;
    }

    @Override
    public FunckyLiteral toExpression() {
        return new FunckyLiteral(engine, this);
    }

    @Override
    public int compareTo(FunckyType type) {
        final int classComparison = super.compareTo(type);
        return (classComparison == 0) ? Integer.compare(hash, ((FunckyTypeVariable) type).hash) : classComparison;
    }

    @Override
    public boolean equals(final Object object) {
        return (object instanceof FunckyTypeVariable) && (hash == ((FunckyTypeVariable) object).hash);
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
