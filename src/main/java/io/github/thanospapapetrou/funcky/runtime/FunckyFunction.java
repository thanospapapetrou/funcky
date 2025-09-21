package io.github.thanospapapetrou.funcky.runtime;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;

public non-sealed abstract class FunckyFunction extends FunckyValue implements Comparable<FunckyFunction> {
    protected final FunckyFunctionType type;

    protected FunckyFunction(final FunckyFunctionType type) {
        this.type = type;
    }

    public abstract FunckyValue apply(final FunckyExpression argument, final ScriptContext context);

    @Override
    public FunckyFunctionType getType() {
        return type;
    }

    @Override
    public int compareTo(final FunckyFunction function) {
        return toString().compareTo(function.toString());
    }

    @Override
    public boolean equals(final Object object) {
        return (object instanceof FunckyFunction) && toString().equals(object.toString());
    }

    @Override
    public int hashCode() {
        return toString().hashCode();
    }
}
