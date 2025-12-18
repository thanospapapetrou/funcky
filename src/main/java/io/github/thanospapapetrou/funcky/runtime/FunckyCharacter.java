package io.github.thanospapapetrou.funcky.runtime;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType;

public final class FunckyCharacter extends FunckyValue {
    private final char value;

    public FunckyCharacter(final FunckyEngine engine, final char value) {
        super(engine);
        this.value = value;
    }

    public char getValue() {
        return value;
    }

    @Override
    public FunckySimpleType getType() {
        return FunckySimpleType.CHARACTER.apply(engine);
    }

    @Override
    public FunckyLiteral toExpression() {
        return new FunckyLiteral(engine, this);
    }

    @Override
    public int compareTo(final FunckyValue value) {
        return (value instanceof FunckyCharacter character) ? Character.compare(this.value, character.value)
                : super.compareTo(value);
    }

    @Override
    public int hashCode() {
        return Character.hashCode(value);
    }

    @Override
    public String toString() {
        return Character.toString(value);
    }
}
