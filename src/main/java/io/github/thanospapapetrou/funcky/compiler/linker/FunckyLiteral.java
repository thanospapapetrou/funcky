package io.github.thanospapapetrou.funcky.compiler.linker;

import java.net.URI;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.parser.EscapeHelper;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;

import static io.github.thanospapapetrou.funcky.runtime.FunckyListType.STRING;
import static io.github.thanospapapetrou.funcky.runtime.FunckySimpleType.CHARACTER;

public final class FunckyLiteral extends FunckyExpression {
    private static final String FORMAT_CHARACTER = "'%1$s'";
    private static final String FORMAT_STRING = "\"%1$s\"";

    private final FunckyValue value;

    public FunckyLiteral(final FunckyEngine engine, final URI file, final int line, final int column,
            final FunckyValue value) {
        super(engine, file, line, column, value.getType());
        this.value = value;
    }

    public FunckyLiteral(final FunckyEngine engine, final FunckyValue value) {
        super(engine, null, -1, -1, value.getType());
        this.value = value;
    }

    //  TODO check if required without engine
    public FunckyLiteral(final FunckyValue value) {
        this(null, value);
    }

    @Override
    public FunckyValue eval(final ScriptContext context) {
        return value;
    }

    @Override
    public String toString() {
        if (value.getType().equals(CHARACTER)) {
            return String.format(FORMAT_CHARACTER, EscapeHelper.escape(value.toString()));
        }
        if (value.getType().equals(STRING)) {
            return String.format(FORMAT_STRING, EscapeHelper.escape(value.toString()));
        }
        if (value.getType() instanceof FunckyListType) {
            return ((FunckyList) value).toString(FunckyExpression::toString);
        }
        return value.toString();
    }
}
