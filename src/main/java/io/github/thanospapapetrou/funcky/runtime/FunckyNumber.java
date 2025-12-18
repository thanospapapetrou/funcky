package io.github.thanospapapetrou.funcky.runtime;

import java.math.BigDecimal;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType;

public final class FunckyNumber extends FunckyValue {
    private final BigDecimal value;

    public FunckyNumber(final FunckyEngine engine, final BigDecimal value) {
        super(engine);
        this.value = value;
    }

    public BigDecimal getValue() {
        return value;
    }

    @Override
    public FunckySimpleType getType() {
        return FunckySimpleType.NUMBER.apply(engine);
    }

    @Override
    public FunckyLiteral toExpression() {
        return new FunckyLiteral(engine, this);
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
