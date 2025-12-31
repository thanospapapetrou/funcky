package io.github.thanospapapetrou.funcky.runtime;

import java.util.function.Function;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.runtime.prelude.Booleans;
import io.github.thanospapapetrou.funcky.runtime.prelude.FunckyLibrary;
import io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.BOOLEAN;

public final class FunckyBoolean extends FunckyValue {
    public static final Function<FunckyContext, FunckyBoolean> FALSE = context -> new FunckyBoolean(context, false);
    public static final Function<FunckyContext, FunckyBoolean> TRUE = context -> new FunckyBoolean(context, true);

    private final boolean value;

    private FunckyBoolean(final FunckyContext context, final boolean value) {
        super(context);
        this.value = value;
    }

    public boolean getValue() {
        return value;
    }

    @Override
    public FunckySimpleType getType() {
        return BOOLEAN.apply(context);
    }

    @Override
    public FunckyReference toExpression() {
        return new FunckyReference(context.getEngine(), FunckyLibrary.getNamespace(Booleans.class),
                Boolean.toString(value));
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
