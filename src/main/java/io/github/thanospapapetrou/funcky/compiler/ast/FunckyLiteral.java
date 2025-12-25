package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;
import java.util.stream.Collectors;

import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.compiler.parser.EscapeHelper;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.FunckyRecord;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;

public final class FunckyLiteral extends FunckyExpression {
    private static final String FORMAT_CHARACTER = "'%1$s'";
    private static final String FORMAT_STRING = "\"%1$s\"";

    private final FunckyValue value;

    public FunckyLiteral(final URI file, final int line, final int column,
            final FunckyValue value) {
        super(file, line, column);
        this.value = value;
    }

    public FunckyLiteral(final FunckyValue value) {
        this(null, -1, -1, value);
    }

    public FunckyValue getValue() {
        return value;
    } // TODO remove and replace with eval

    @Override
    public FunckyType getType(final FunckyContext context) {
        return value.getType();
    }

    @Override
    public FunckyValue eval(final FunckyContext context) {
        return value;
    }

    @Override
    public String toString(final boolean canonical, final FunckyContext context) {
        if (value.getType().equals(FunckySimpleType.CHARACTER.apply(context.getEngine()))) {
            return String.format(FORMAT_CHARACTER, EscapeHelper.escape(value.toString()));
        } else if (value.getType().equals(FunckyListType.STRING.apply(context.getEngine()))) {
            return String.format(FORMAT_STRING, EscapeHelper.escape(value.toString()));
        } else if (value instanceof FunckyList list) {
            final StringBuilder string = new StringBuilder(FunckyList.PREFIX);
            for (FunckyList l = list; l.getTail() != null; l = (FunckyList) l.getTail().eval(context)) {
                string.append(l.getHead().toString(canonical, context)).append(FunckyList.DELIMITER);
            }
            if (string.length() > FunckyList.PREFIX.length()) {
                string.setLength(string.length() - FunckyList.DELIMITER.length());
            }
            return string.append(FunckyList.SUFFIX).toString();
        } else if (value instanceof FunckyRecord record) {
            return FunckyRecord.PREFIX + record.getComponents().stream()
                    .map(component -> component.toString(canonical, context))
                    .collect(Collectors.joining(FunckyRecord.DELIMITER)) + FunckyRecord.SUFFIX;
        }
        return value.toString();
    }
}
