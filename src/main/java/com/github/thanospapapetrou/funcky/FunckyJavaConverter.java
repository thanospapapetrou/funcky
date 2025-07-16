package com.github.thanospapapetrou.funcky;

import java.math.BigDecimal;
import java.util.Iterator;
import java.util.List;

import com.github.thanospapapetrou.funcky.runtime.FunckyBoolean;
import com.github.thanospapapetrou.funcky.runtime.FunckyCharacter;
import com.github.thanospapapetrou.funcky.runtime.FunckyList;
import com.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import com.github.thanospapapetrou.funcky.runtime.FunckyValue;
import com.github.thanospapapetrou.funcky.runtime.types.FunckyListType;
import com.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

public class FunckyJavaConverter {
    // TODO convert types?

    public FunckyNumber convert(final Number number) {
        return new FunckyNumber(new BigDecimal(number.toString()));
    }

    public BigDecimal convert(final FunckyNumber number) {
        return number.getValue();
    }

    public FunckyBoolean convert(final boolean bool) {
        return bool ? FunckyBoolean.TRUE : FunckyBoolean.FALSE;
    }

    public boolean convert(final FunckyBoolean bool) {
        return bool.getValue();
    }

    public FunckyCharacter convert(final char character) {
        return new FunckyCharacter(character);
    }

    public char convert(final FunckyCharacter character) {
        return character.getValue();
    }

    // TODO convert functions?

    public FunckyList convert(final Iterable<?> iterable) {
        return convert(iterable.iterator());
    }

    public FunckyList convert(final String string) {
        return new FunckyList(FunckyListType.STRING,
                string.isEmpty() ? null : convert(string.charAt(0)),
                string.isEmpty() ? null : convert(string.substring(1)));
    }

    public List<?> convert(final FunckyList list) {
        return null; // TODO
    }

    private FunckyList convert(final Iterator<?> iterator) {
        // TODO check types of head vs tail
        final FunckyValue head = iterator.hasNext() ? convert(iterator.next()) : null;
        return new FunckyList(new FunckyListType((head == null) ? new FunckyTypeVariable() : head.getType()),
                head, (head == null) ? null : convert(iterator));
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
        } else if (object instanceof Iterable<?>) {
            return convert((Iterable<?>) object);
        } else if (object instanceof String) {
            return convert((String) object);
        } else {
            return null; // TODO
        }
    }
}
