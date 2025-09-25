package io.github.thanospapapetrou.funcky.runtime;

import java.util.function.Function;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.runtime.prelude.Booleans;

public final class FunckyBoolean extends FunckyValue implements Comparable<FunckyBoolean> {
    public static final Function<FunckyEngine, FunckyBoolean> FALSE = engine -> new FunckyBoolean(engine, false);
    public static final Function<FunckyEngine, FunckyBoolean> TRUE = engine -> new FunckyBoolean(engine, true);

    private final boolean value;

    private FunckyBoolean(final FunckyEngine engine, final boolean value) {
        super(engine);
        this.value = value;
    }

    public boolean getValue() {
        return value;
    }

    @Override
    public FunckySimpleType getType() {
        return FunckySimpleType.BOOLEAN.apply(engine);
    }

    @Override
    public FunckyReference toExpression() {
        return new FunckyReference(engine, new Booleans(engine).getFile(), Boolean.toString(value));
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
