package io.github.thanospapapetrou.funcky;

import java.math.BigDecimal;
import java.util.Iterator;
import java.util.stream.Stream;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.FunckyBoolean;
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;

public class FunckyJavaConverter {
    private static final String ERROR_CONVERTING_TO_FUNCKY = "Error converting %1$s (%2$s) to %3$s";
    private static final String ERROR_CONVERTING_TO_JAVA = "Error converting %1$s (%2$s) to Java";

    // TODO convert types?

    private final FunckyEngine engine;

    public FunckyJavaConverter(final FunckyEngine engine) {
        this.engine = engine;
    }

    public FunckyNumber convert(final Number number) {
        return new FunckyNumber(engine, new BigDecimal(number.toString()));
    }

    public BigDecimal convert(final FunckyNumber number) {
        return number.getValue();
    }

    public FunckyBoolean convert(final boolean bool) {
        return bool ? FunckyBoolean.TRUE.apply(engine) : FunckyBoolean.FALSE.apply(engine);
    }

    public boolean convert(final FunckyBoolean bool) {
        return bool.getValue();
    }

    public FunckyCharacter convert(final char character) {
        return new FunckyCharacter(engine, character);
    }

    public char convert(final FunckyCharacter character) {
        return character.getValue();
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
        final FunckyType headType = (head == null) ? new FunckyTypeVariable(engine) : head.getType();
        final FunckyListType tailType =
                (tail == null) ? new FunckyListType(engine, new FunckyLiteral(engine, new FunckyTypeVariable(engine))) : tail.getType();
        final String error = String.format(ERROR_CONVERTING_TO_FUNCKY, iterator.getClass().getName(), iterator,
                engine.getFactory().getLanguageName());
        final FunckyListType listType = (FunckyListType) new FunckyListType(engine, new FunckyLiteral(engine, headType)).unify(tailType);
            if (listType == null) {
                throw new IllegalArgumentException(error);
            }
        return new FunckyList(engine, listType, (head == null) ? null : new FunckyLiteral(engine, head),
                (tail == null) ? null : new FunckyLiteral(engine, tail));
    }

    public FunckyList convert(final String string) {
        return new FunckyList(engine, FunckyListType.STRING.apply(engine),
                string.isEmpty() ? null : new FunckyLiteral(engine, convert(string.charAt(0))),
                string.isEmpty() ? null : new FunckyLiteral(engine, convert(string.substring(1))));
    }

    public Stream<?> convert(final FunckyList list) {
        // TODO
        return null;
    }

    // TODO convert records
    // TODO convert functions?

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
                String.format(ERROR_CONVERTING_TO_FUNCKY, object.getClass().getName(), object,
                        engine.getFactory().getLanguageName()));
    }

    private Object convert(final FunckyValue value) {
        if (value instanceof FunckyNumber) {
            return convert((FunckyNumber) value);
        } else if (value instanceof FunckyBoolean) {
            return convert((FunckyBoolean) value);
        } else if (value instanceof FunckyCharacter) {
            return convert((FunckyCharacter) value);
        } else if (value instanceof FunckyList) {
            return convert((FunckyList) value);
        }
        // TODO continue with other types
        throw new IllegalArgumentException(String.format(ERROR_CONVERTING_TO_JAVA, value.getType(), value));
    }
}
