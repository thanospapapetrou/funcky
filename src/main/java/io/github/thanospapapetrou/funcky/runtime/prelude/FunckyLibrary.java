package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.math.BigDecimal;
import java.net.URI;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.List;
import java.util.stream.Collectors;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyScript;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;

public sealed class FunckyLibrary extends FunckyScript
        permits Types, Numbers, Booleans, Characters, Lists, Commons, Combinators {
    private final FunckyValue[] values;

    protected static int requireInt(final FunckyNumber number, final String message) throws FunckyRuntimeException {
        if ((number.getValue().compareTo(BigDecimal.valueOf(number.getValue().intValue())) != 0)) {
            throw new FunckyRuntimeException(message);
        }
        return number.getValue().intValue();
    }

    protected static <E extends Enum<E>> E requireEnum(final FunckyNumber number, final Class<E> enumeration,
            final String message) throws FunckyRuntimeException {
        final int ordinal = requireInt(number, message);
        return EnumSet.allOf(enumeration).stream()
                .filter(value -> value.ordinal() == ordinal)
                .findFirst()
                .orElseThrow(() -> new FunckyRuntimeException(message));
    }

    public FunckyLibrary(final FunckyValue... values) {
        super(null, null);
        this.values = values;
    }

    @Override
    public URI getFile() {
        return Linker.getNamespace(getClass());
    }

    @Override
    public List<FunckyDefinition> getDefinitions() {
        return Arrays.stream(values)
                .map(value -> new FunckyDefinition(getFile(), -1, ((FunckyReference) value.toExpression()).getName(),
                        new FunckyLiteral(value)))
                .collect(Collectors.toList());
    }
}
