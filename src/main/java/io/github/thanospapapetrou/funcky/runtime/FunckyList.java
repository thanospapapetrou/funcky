package io.github.thanospapapetrou.funcky.runtime;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyListType;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.LIST;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.STRING;

public final class FunckyList extends FunckyValue {
    public static final String DELIMITER = ", ";
    public static final String PREFIX = "[";
    public static final String SUFFIX = "]";

    private final FunckyListType type;
    private final FunckyExpression head;
    private final FunckyExpression tail;

    public FunckyList(final FunckyContext context, final Object type, final FunckyExpression head,
            final FunckyExpression tail) {
        this(context, LIST(type).apply(context), head, tail);
    }

    private FunckyList(final FunckyContext context, final FunckyListType type, final FunckyExpression head,
            final FunckyExpression tail) {
        super(context);
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
    public int compareTo(final FunckyValue value) {
        if (value instanceof FunckyList list) {
            final int comparison = (head == null) ? ((list.head == null) ? 0 : -1) : ((list.head == null) ? 1
                    : head.eval(context).compareTo(list.head.eval(context)));
            return (comparison == 0) ? ((tail == null) ? ((list.tail == null) ? 0 : -1) : ((list.tail == null) ? 1
                    : tail.eval(context).compareTo(list.tail.eval(context)))) : comparison;
        }
        return super.compareTo(value);
    }

    @Override
    public int hashCode() {
        return ((head == null) ? 0 : head.eval(context).hashCode()) + ((tail == null) ? 0
                : tail.eval(context).hashCode());
    }

    @Override
    public String toString() {
        final boolean isString = type.equals(STRING.apply(context));
        final StringBuilder string = new StringBuilder(isString ? "" : PREFIX);
        for (FunckyList list = this; list.tail != null; list = (FunckyList) list.tail.eval(context)) {
            string.append(list.head.eval(context).toString()).append(isString ? "" : DELIMITER);
        }
        if ((!isString) && (string.length() > PREFIX.length())) {
            string.setLength(string.length() - DELIMITER.length());
        }
        return string.append(isString ? "" : SUFFIX).toString();
    }
}
