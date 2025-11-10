package io.github.thanospapapetrou.funcky;

import java.math.BigDecimal;
import java.util.Iterator;
import java.util.stream.Stream;

import io.github.thanospapapetrou.funcky.compiler.linker.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.FunckyBoolean;
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;

import static io.github.thanospapapetrou.funcky.runtime.FunckyListType.LIST;
import static io.github.thanospapapetrou.funcky.runtime.FunckyListType.STRING;

public class FunckyJavaConverter {
    private static final String ERROR_CONVERTING_TO_FUNCKY = "Error converting %1$s (%2$s) to Funcky";

    public FunckyNumber convert(final Number number) {
        return new FunckyNumber(new BigDecimal(number.toString()));
    }

    public FunckyBoolean convert(final boolean bool) {
        return bool ? FunckyBoolean.TRUE : FunckyBoolean.FALSE;
    }

    public FunckyCharacter convert(final char character) {
        return new FunckyCharacter(character);
    }

    public FunckyList convert(final Stream<?> stream) {
        return convert(stream.iterator());
    }

    public FunckyList convert(final Iterable<?> iterable) {
        return convert(iterable.iterator());
    }

    public FunckyList convert(final Iterator<?> iterator) {
        final FunckyValue head = iterator.hasNext() ? convert(iterator.next()) : null;
        final FunckyList tail = (head == null) ? null : convert(iterator);
        final FunckyType headType = (head == null) ? new FunckyTypeVariable() : head.getType();
        final FunckyListType tailType =
                (tail == null) ? LIST(new FunckyTypeVariable()) : tail.getType();
        final String error = String.format(ERROR_CONVERTING_TO_FUNCKY, iterator.getClass().getName(), iterator);
        final FunckyListType listType =
                (FunckyListType) LIST(headType).unify(tailType);
            if (listType == null) {
                throw new IllegalArgumentException(error);
            }
        return new FunckyList(listType, (head == null) ? null : new FunckyLiteral(head),
                (tail == null) ? null : new FunckyLiteral(tail));
    }

    public FunckyList convert(final String string) {
        return new FunckyList(STRING,
                string.isEmpty() ? null : new FunckyLiteral(convert(string.charAt(0))),
                string.isEmpty() ? null : new FunckyLiteral(convert(string.substring(1))));
    }

    private FunckyValue convert(final Object object) {
        if (object instanceof FunckyValue) {
            return (FunckyValue) object;
        } else if (object instanceof Number) {
            return convert((Number) object);
        } else if (object instanceof Boolean) {
            return convert((boolean) object);
        } else if (object instanceof Character) {
            return convert((char) object);
        } else if (object instanceof Stream<?>) {
            return convert((Stream<?>) object);
        } else if (object instanceof Iterable<?>) { // TODO add support for Maps
            return convert((Iterable<?>) object);
        } else if (object instanceof Iterator<?>) {
            return convert((Iterator<?>) object);
        } else if (object instanceof String) {
            return convert((String) object);
        }
        throw new IllegalArgumentException(
                String.format(ERROR_CONVERTING_TO_FUNCKY, object.getClass().getName(), object));
    }
}
