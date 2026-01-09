package io.github.thanospapapetrou.funcky.runtime;

import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.CHARACTER;

public final class FunckyCharacter extends FunckyValue {
    private final char value;

    public FunckyCharacter(final FunckyContext context, final char value) {
        super(context);
        this.value = value;
    }

    public char getValue() {
        return value;
    }

    @Override
    public FunckySimpleType getType() {
        return CHARACTER.apply(context);
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
