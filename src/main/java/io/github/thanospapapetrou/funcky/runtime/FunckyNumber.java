package io.github.thanospapapetrou.funcky.runtime;

import java.math.BigDecimal;

import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.NUMBER;

public final class FunckyNumber extends FunckyValue {
    private final BigDecimal value;

    public FunckyNumber(final FunckyContext context, final BigDecimal value) {
        super(context);
        this.value = value;
    }

    public BigDecimal getValue() {
        return value;
    }

    @Override
    public FunckySimpleType getType() {
        return NUMBER.apply(context);
    }

    @Override
    public int compareTo(final FunckyValue value) {
        return (value instanceof FunckyNumber number) ? this.value.compareTo(number.value) : super.compareTo(value);
    }

    @Override
    public int hashCode() {
        return Double.hashCode(value.doubleValue());
    }

    @Override
    public String toString() {
        return value.toString();
    }
}
