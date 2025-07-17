package io.github.thanospapapetrou.funcky.runtime;

import io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.runtime.prelude.Booleans;

public class FunckyBoolean extends FunckyValue implements Comparable<FunckyBoolean> {
    public static final FunckyBoolean FALSE = new FunckyBoolean(false);
    public static final FunckyBoolean TRUE = new FunckyBoolean(true);

    private final boolean value;

    private FunckyBoolean(final boolean value) {
        this.value = value;
    }

    public boolean getValue() {
        return value;
    }

    @Override
    public FunckySimpleType getType() {
        return FunckySimpleType.BOOLEAN;
    }

    @Override
    public FunckyReference toExpression() {
        return new FunckyReference(Booleans.class, Boolean.toString(value));
    }

    @Override
    public int compareTo(final FunckyBoolean bool) {
        return Boolean.compare(value, bool.value);
    }

    @Override
    public boolean equals(final Object object) {
        return (object instanceof FunckyBoolean) && (value == ((FunckyBoolean) object).value);
    }

    @Override
    public int hashCode() {
        return Boolean.hashCode(value);
    }
}
