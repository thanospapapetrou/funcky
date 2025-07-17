package io.github.thanospapapetrou.funcky.runtime.types;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;

public class FunckyTypeVariable extends FunckyType {
    private static final AtomicInteger HASH = new AtomicInteger();
    private static final String TYPE_VARIABLE = "$_%1$x";

    private final int hash;

    public FunckyTypeVariable() {
        this(HASH.getAndIncrement());
    }

    private FunckyTypeVariable(final int hash) {
        this.hash = hash;
    }

    @Override
    public FunckyLiteral toExpression() {
        return new FunckyLiteral(this);
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
        return String.format(TYPE_VARIABLE, hash);
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
