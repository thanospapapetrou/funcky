package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.math.BigDecimal;
import java.net.URI;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyScript;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;

public sealed class FunckyLibrary extends FunckyScript
        permits Types, Numbers, Booleans, Characters, Lists, Commons, Combinators {

    protected static int requireInt(final FunckyNumber number, final String message) {
        if ((number.getValue().compareTo(BigDecimal.valueOf(number.getValue().intValue())) != 0)) {
            throw new SneakyRuntimeException(message);
        }
        return number.getValue().intValue();
    }

    protected static <E extends Enum<E>> E requireEnum(final FunckyNumber number, final Class<E> enumeration,
            final String message) {
        final int ordinal = requireInt(number, message);
        return EnumSet.allOf(enumeration).stream()
                .filter(value -> value.ordinal() == ordinal)
                .findFirst()
                .orElseThrow(() -> new SneakyRuntimeException(message));
    }

    protected FunckyLibrary(final FunckyEngine engine, final FunckyValue... values) { // TODO remove values
        super(engine, null);
    }

    @Override
    public URI getFile() {
        return Linker.getNamespace(getClass());
    }

    @Override
    public List<FunckyDefinition> getDefinitions() {
        return Arrays.stream(getClass().getDeclaredFields())
                .map(this::getDefinition)
                .filter(Objects::nonNull)
                .toList();
    }

    private FunckyDefinition getDefinition(final Field field) {
        try {
            final int modifiers = field.getModifiers();
            return (Modifier.isPublic(modifiers) && Modifier.isFinal(modifiers) && field.getName().startsWith("$"))
                    // TODO improve starts with
                    ? new FunckyDefinition(getFile(), -1, field.getName().substring(1), // TODO improve substring
                    new FunckyLiteral(engine, (FunckyValue) field.get(this))) : null;
        } catch (final IllegalAccessException e) {
            return null;
        }
    }
}
