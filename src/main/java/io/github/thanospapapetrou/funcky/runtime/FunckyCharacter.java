package io.github.thanospapapetrou.funcky.runtime;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.parser.EscapeHelper;

public final class FunckyCharacter extends FunckyValue implements Comparable<FunckyCharacter> {
    private static final String JAVA = "new %1$s(engine, '%2$s')";

    private final char value;

    public FunckyCharacter(final FunckyEngine engine, final char value) {
        super(engine);
        this.value = value;
    }

    public char getValue() {
        return value;
    }

    @Override
    public String toJava() {
        return String.format(JAVA, FunckyCharacter.class.getName(), EscapeHelper.escape(Character.toString(value)));
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
