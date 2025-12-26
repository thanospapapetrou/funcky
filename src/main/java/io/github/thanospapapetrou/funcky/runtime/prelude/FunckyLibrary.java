package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.math.BigDecimal;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.EnumSet;
import java.util.Locale;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;

public sealed class FunckyLibrary permits Types, Numbers, Booleans, Characters, Lists, Commons, Combinators {
    private static final String ERROR_RESOLVING_NAMESPACE = "Error resolving namespace for library `%1$s`";

    protected final FunckyEngine engine;

    public static URI getNamespace(final Class<? extends FunckyLibrary> library) {
        try {
            return new URI(Linker.PRELUDE_SCHEME, library.getSimpleName().toLowerCase(Locale.ROOT), null); // TODO
        } catch (final URISyntaxException e) {
            throw new IllegalStateException(String.format(ERROR_RESOLVING_NAMESPACE, library.getName()), e);
        }
    }

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
        this.engine = engine;
    }
}
