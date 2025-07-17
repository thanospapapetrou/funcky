package io.github.thanospapapetrou.funcky.runtime;

import io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;

public class FunckyCharacter extends FunckyValue implements Comparable<FunckyCharacter> {
    private final char value;

    public FunckyCharacter(final char value) {
        this.value = value;
    }

    public char getValue() {
        return value;
    }

    @Override
    public FunckySimpleType getType() {
        return FunckySimpleType.CHARACTER;
    }

    @Override
    public FunckyLiteral toExpression() {
        return new FunckyLiteral(this);
    }

    @Override
    public int compareTo(final FunckyCharacter character) {
        return Character.compare(value, character.value);
    }

    @Override
    public boolean equals(final Object object) {
        return (object instanceof FunckyCharacter) && (value == ((FunckyCharacter) object).value);
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
