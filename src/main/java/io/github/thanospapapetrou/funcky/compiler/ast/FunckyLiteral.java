package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;
import java.util.stream.Collectors;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.compiler.parser.EscapeHelper;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.FunckyRecord;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.STRING;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.CHARACTER;

public non-sealed class FunckyLiteral extends FunckyExpression {
    private static final String FORMAT_CHARACTER = "'%1$s'";
    private static final String FORMAT_STRING = "\"%1$s\"";

    private final FunckyValue value;

    public FunckyLiteral(final URI file, final int line, final int column,
            final FunckyValue value) {
        this(value.getContext().getEngine(), file, line, column, value);
    }

    public FunckyLiteral(final FunckyValue value) {
        this(null, -1, -1, value);
    }

    private FunckyLiteral(final FunckyEngine engine, final URI file, final int line, final int column,
            final FunckyValue value) {
        super(engine, file, line, column);
        this.value = value;
    }

    @Override
    public FunckyType getType() {
        return value.getType();
    }

    @Override
    public FunckyValue eval(final FunckyContext context) {
        return value;
    }

    @Override
    public String toString(final boolean canonical) {
        if (value.getType().equals(CHARACTER.apply(engine.getContext()))) {
            return String.format(FORMAT_CHARACTER, EscapeHelper.escape(value.toString()));
        } else if (value.getType().equals(STRING.apply(engine.getContext()))) {
            return String.format(FORMAT_STRING, EscapeHelper.escape(value.toString()));
        } else if (value instanceof FunckyList list) {
            final StringBuilder string = new StringBuilder(FunckyList.PREFIX);
            for (FunckyList l = list; l.getTail() != null; l = (FunckyList) l.getTail().eval(engine.getContext())) {
                string.append(l.getHead().toString(canonical)).append(FunckyList.DELIMITER);
            }
            if (string.length() > FunckyList.PREFIX.length()) {
                string.setLength(string.length() - FunckyList.DELIMITER.length());
            }
            return string.append(FunckyList.SUFFIX).toString();
        } else if (value instanceof FunckyRecord record) {
            return FunckyRecord.PREFIX + record.getComponents().stream()
                    .map(component -> component.toString(canonical))
                    .collect(Collectors.joining(FunckyRecord.DELIMITER)) + FunckyRecord.SUFFIX;
        }
        return value.toString();
    }
}
