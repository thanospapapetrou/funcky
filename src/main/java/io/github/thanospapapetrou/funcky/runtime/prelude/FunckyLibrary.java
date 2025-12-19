package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.math.BigDecimal;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.EnumSet;
import java.util.Locale;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.FunckyFactory;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;

public sealed class FunckyLibrary permits Types, Numbers, Booleans, Characters, Lists, Commons, Combinators {
    protected final FunckyEngine engine;

    public static URI getNamespace(final Class<? extends FunckyLibrary> library) {
        try {
            return new URI("funcky", library.getSimpleName().toLowerCase(Locale.ROOT), null); // TODO
        } catch (final URISyntaxException e) {
            throw new RuntimeException(e); // TODO
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
