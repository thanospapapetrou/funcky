package io.github.thanospapapetrou.funcky.runtime;

import java.util.function.Function;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;

public final class FunckyList extends FunckyValue implements Comparable<FunckyList> {
    private static final String DELIMITER = ", ";
    private static final String PREFIX = "[";
    private static final String SUFFIX = "]";

    private final FunckyListType type;
    private final FunckyExpression head;
    private final FunckyExpression tail;

    private static String toString(final FunckyExpression expression) {
        return expression.eval(expression.getEngine().getContext()).toString();
    }

    public FunckyList(final FunckyEngine engine, final FunckyListType type, final FunckyExpression head,
            final FunckyExpression tail) {
        super(engine);
        this.type = type;
        this.head = head;
        this.tail = tail;
    }

    public FunckyExpression getHead() {
        return head;
    }

    public FunckyExpression getTail() {
        return tail;
    }

    public String toString(final Function<FunckyExpression, String> toString) {
        final StringBuilder string = new StringBuilder(PREFIX);
        for (FunckyList list = this; list.tail != null; list = (FunckyList) list.tail.eval(engine.getContext())) {
            string.append(toString.apply(list.head)).append(DELIMITER);
        }
        if (string.length() > PREFIX.length()) {
            string.setLength(string.length() - DELIMITER.length());
        }
        return string.append(SUFFIX).toString();
    }

    @Override
    public FunckyListType getType() {
        return type;
    }

    @Override
    public FunckyLiteral toExpression() {
        return new FunckyLiteral(engine, this);
    }

    @Override
    public int compareTo(final FunckyList list) {
        final int headComparison = (head == null) ? ((list.head == null) ? 0 : -1) : ((list.head == null) ? 1
                : ((Comparable<FunckyValue>) head.eval(engine.getContext())).compareTo(
                        list.head.eval(engine.getContext())));
        return (headComparison == 0) ? ((tail == null) ? ((list.tail == null) ? 0 : -1) : ((list.tail == null) ? 1
                : ((Comparable<FunckyValue>) tail.eval(engine.getContext())).compareTo(
                        list.tail.eval(engine.getContext()))))
                    : headComparison;
    }

    @Override
    public boolean equals(final Object object) {
            return (object instanceof FunckyList) && ((head == null) ? (((FunckyList) object).head == null)
                    : ((((FunckyList) object).head != null) && head.eval(engine.getContext())
                            .equals(((FunckyList) object).head.eval(engine.getContext()))))
                    && ((tail == null) ? (((FunckyList) object).tail == null)
                    : ((((FunckyList) object).tail != null) && tail.eval(engine.getContext())
                            .equals(((FunckyList) object).tail.eval(engine.getContext()))));
    }

    @Override
    public int hashCode() {
        return ((head == null) ? 0 : head.eval(engine.getContext()).hashCode()) + ((tail == null) ? 0
                : tail.eval(engine.getContext()).hashCode());
    }

    @Override
    public String toString() {
        if (type.equals(FunckyListType.STRING.apply(engine))) {
                final StringBuilder string = new StringBuilder();
            for (FunckyList list = this; list.tail != null; list = (FunckyList) list.tail.eval(engine.getContext())) {
                string.append(((FunckyCharacter) list.head.eval(engine.getContext())).getValue());
                }
                return string.toString();
            }
            return toString(FunckyList::toString);
    }
}
