package io.github.thanospapapetrou.funcky.runtime;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;

public sealed abstract class FunckyValue implements Comparable<FunckyValue>
        permits FunckyType, FunckyNumber, FunckyBoolean, FunckyCharacter, FunckyFunction, FunckyList, FunckyRecord,
        FunckyMonad {
    protected final FunckyContext context;

    protected FunckyValue(final FunckyContext context) {
        this.context = context;
    }

    public FunckyContext getContext() {
        return context;
    }

    public abstract FunckyType getType();

    public abstract FunckyExpression toExpression();

    @Override
    public int compareTo(final FunckyValue value) {
        return getType().compareTo(value.getType());
    }

    @Override
    public boolean equals(final Object object) {
        return (object instanceof FunckyValue value) && (compareTo(value) == 0);
    }

    @Override
    public String toString() {
        return toExpression().toString(true);
    }
}
