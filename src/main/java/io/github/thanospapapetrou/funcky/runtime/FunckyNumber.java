package io.github.thanospapapetrou.funcky.runtime;

import java.math.BigDecimal;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;

public final class FunckyNumber extends FunckyValue implements Comparable<FunckyNumber> {
    private static final String JAVA = "new %1$s(engine, new %2$s(\"%3$f\"))"; // TODO improve argument ordering here and in other JAVAs

    private final BigDecimal value;

    public FunckyNumber(final FunckyEngine engine, final BigDecimal value) {
        super(engine);
        this.value = value;
    }

    public BigDecimal getValue() {
        return value;
    }

    @Override
    public String toJava() {
        return String.format(JAVA, FunckyNumber.class.getName(), BigDecimal.class.getName(), value);
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
    public int compareTo(final FunckyNumber number) {
        return value.compareTo(number.value);
    }

    @Override
    public boolean equals(final Object object) {
        return (object instanceof FunckyNumber) && (compareTo((FunckyNumber) object) == 0);
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
