package io.github.thanospapapetrou.funcky.runtime;

import java.util.function.Function;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.runtime.prelude.Booleans;
import io.github.thanospapapetrou.funcky.runtime.prelude.FunckyLibrary;
import io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType;

public final class FunckyBoolean extends FunckyValue {
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
        return new FunckyReference(engine, FunckyLibrary.getNamespace(Booleans.class), Boolean.toString(value));
    }

    @Override
    public int compareTo(final FunckyValue value) {
        return (value instanceof FunckyBoolean bool) ? Boolean.compare(this.value, bool.value) : super.compareTo(value);
    }

    @Override
    public int hashCode() {
        return Boolean.hashCode(value);
    }
}
