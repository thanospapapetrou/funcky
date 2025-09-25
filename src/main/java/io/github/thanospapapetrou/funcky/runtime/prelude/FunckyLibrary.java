package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.math.BigDecimal;
import java.net.URI;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.List;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyScript;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;

public sealed class FunckyLibrary extends FunckyScript
        permits Types, Numbers, Booleans, Characters, Lists, Commons, Combinators {
    private static final String ERROR_RESOLVING_FIELD = "Error resolving field %s";

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

    protected FunckyLibrary(final FunckyEngine engine) {
        super(engine, null);
    }

    @Override
    public URI getFile() {
        return Linker.getNamespace(getClass());
    }

    @Override
    public List<FunckyDefinition> getDefinitions() {
        return Arrays.stream(getClass().getDeclaredFields()).filter(field -> Modifier.isPublic(field.getModifiers()))
                .filter(field -> field.getName().startsWith(Linker.JAVA_PREFIX))
                .map(this::getDefinition)
                .toList();
    }

    private FunckyDefinition getDefinition(final Field field) {
        try {
            return new FunckyDefinition(getFile(), -1, field.getName().substring(Linker.JAVA_PREFIX.length()),
                    new FunckyLiteral(engine, (FunckyValue) field.get(this)));
        } catch (final IllegalAccessException e) {
            throw new IllegalStateException(String.format(ERROR_RESOLVING_FIELD, field.getName()), e);
        }
    }
}
