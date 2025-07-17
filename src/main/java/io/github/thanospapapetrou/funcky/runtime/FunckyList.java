package io.github.thanospapapetrou.funcky.runtime;

import java.util.function.Function;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyFunckyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyListType;

public class FunckyList extends FunckyValue implements Comparable<FunckyList> {
    private static final String DELIMITER = ", ";
    private static final String PREFIX = "[";
    private static final String SUFFIX = "]";

    private final FunckyListType type;
    private final FunckyExpression head;
    private final FunckyExpression tail;

    private static String toString(final FunckyExpression expression) {
        try {
            return expression.eval().toString();
        } catch (final FunckyRuntimeException e) {
            throw new SneakyFunckyRuntimeException(e);
        }
    }

    public FunckyList(final FunckyListType type, final FunckyExpression head, final FunckyExpression tail) {
        this.type = type;
        this.head = head;
        this.tail = tail;
    }

    public FunckyList(final FunckyListType type, final FunckyValue head, final FunckyList tail) {
        this(type, (head == null) ? null : new FunckyLiteral(head), (tail == null) ? null : new FunckyLiteral(tail));
    }

    public FunckyExpression getHead() {
        return head;
    }

    public FunckyExpression getTail() {
        return tail;
    }

    @Override
    public FunckyListType getType() {
        return type;
    }

    @Override
    public FunckyLiteral toExpression() {
        return new FunckyLiteral(this);
    }

    @Override
    public int compareTo(final FunckyList list) {
        try {
            final int headComparison = (head == null) ? ((list.head == null) ? 0 : -1)
                    : ((list.head == null) ? 1 : ((Comparable<FunckyValue>) head.eval()).compareTo(list.head.eval()));
            return (headComparison == 0) ? ((tail == null) ? ((list.tail == null) ? 0 : -1)
                    : ((list.tail == null) ? 1 : ((Comparable<FunckyValue>) tail.eval()).compareTo(list.tail.eval())))
                    : headComparison;
        } catch (final FunckyRuntimeException e) {
            throw new SneakyFunckyRuntimeException(e);
        }
    }

    @Override
    public boolean equals(final Object object) {
        try {
            return (object instanceof FunckyList) && ((head == null) ? (((FunckyList) object).head == null)
                    : ((((FunckyList) object).head != null) && head.eval().equals(((FunckyList) object).head.eval())))
                    && ((tail == null) ? (((FunckyList) object).tail == null)
                    : ((((FunckyList) object).tail != null) && tail.eval().equals(((FunckyList) object).tail.eval())));
        } catch (final FunckyRuntimeException e) {
            throw new SneakyFunckyRuntimeException(e);
        }
    }

    @Override
    public int hashCode() {
        try {
            return ((head == null) ? 0 : head.eval().hashCode()) + ((tail == null) ? 0 : tail.eval().hashCode());
        } catch (final FunckyRuntimeException e) {
            throw new SneakyFunckyRuntimeException(e);
        }
    }

    public String toString(final Function<FunckyExpression, String> toString) {
        try {
            final StringBuilder string = new StringBuilder(PREFIX);
            for (FunckyList list = this; list.tail != null; list = (FunckyList) list.tail.eval()) {
                string.append(toString.apply(list.head)).append(DELIMITER);
            }
            if (string.length() > PREFIX.length()) {
                string.setLength(string.length() - DELIMITER.length());
            }
            return string.append(SUFFIX).toString();
        } catch (final FunckyRuntimeException e) {
            throw new SneakyFunckyRuntimeException(e);
        }
    }

    @Override
    public String toString() {
        try {
            if (type.equals(FunckyListType.STRING)) {
                final StringBuilder string = new StringBuilder();
                for (FunckyList list = this; list.tail != null; list = (FunckyList) list.tail.eval()) {
                    string.append(((FunckyCharacter) list.head.eval()).getValue());
                }
                return string.toString();
            }
            return toString(FunckyList::toString);
        } catch (final FunckyRuntimeException e) {
            throw new SneakyFunckyRuntimeException(e);
        }
    }
}
