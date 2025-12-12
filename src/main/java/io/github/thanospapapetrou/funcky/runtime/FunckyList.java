package io.github.thanospapapetrou.funcky.runtime;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyListType;

public final class FunckyList extends FunckyValue {
    public static final String DELIMITER = ", ";
    public static final String PREFIX = "[";
    public static final String SUFFIX = "]";

    private final FunckyListType type;
    private final FunckyExpression head;
    private final FunckyExpression tail;

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

    @Override
    public FunckyListType getType() {
        return type;
    }

    @Override
    public FunckyLiteral toExpression() {
        return new FunckyLiteral(engine, this);
    }

    @Override
    public int compareTo(final FunckyValue value) {
        if (value instanceof FunckyList list) {
            final int comparison = (head == null) ? ((list.head == null) ? 0 : -1) : ((list.head == null) ? 1
                    : head.eval(engine.getContext()).compareTo(list.head.eval(engine.getContext())));
            return (comparison == 0) ? ((tail == null) ? ((list.tail == null) ? 0 : -1) : ((list.tail == null) ? 1
                    : tail.eval(engine.getContext()).compareTo(list.tail.eval(engine.getContext())))) : comparison;
        }
        return super.compareTo(value);
    }

    @Override
    public boolean equals(final Object object) {
        return (object instanceof FunckyList list) && (compareTo(list) == 0);
    }

    @Override
    public int hashCode() {
        return ((head == null) ? 0 : head.eval(engine.getContext()).hashCode())
                + ((tail == null) ? 0 : tail.eval(engine.getContext()).hashCode());
    }

    @Override
    public String toString() {
        final boolean isString = type.equals(FunckyListType.STRING.apply(engine));
        final StringBuilder string = new StringBuilder(isString ? "" : PREFIX);
        for (FunckyList list = this; list.tail != null; list = (FunckyList) list.tail.eval(engine.getContext())) {
            string.append(list.head.eval(engine.getContext()).toString()).append(isString ? "" : DELIMITER);
        }
        if ((!isString) && (string.length() > PREFIX.length())) {
            string.setLength(string.length() - DELIMITER.length());
        }
        return string.append(isString ? "" : SUFFIX).toString();
    }
}
