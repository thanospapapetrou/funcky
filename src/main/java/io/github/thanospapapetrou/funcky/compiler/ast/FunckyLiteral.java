package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;
import java.util.Map;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.parser.EscapeHelper;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.FunckySimpleType;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;

public final class FunckyLiteral extends FunckyExpression {
    private static final String FORMAT_CHARACTER = "'%1$s'";
    private static final String FORMAT_STRING = "\"%1$s\"";

    private final FunckyValue value;

    public FunckyLiteral(final FunckyEngine engine, final URI file, final int line, final int column,
            final FunckyValue value) {
        super(engine, file, line, column);
        this.value = value;
    }

    public FunckyLiteral(final FunckyEngine engine, final FunckyValue value) {
        this(engine, null, -1, -1, value);
    }

    public FunckyValue getValue() {
        return value;
    }

    @Override
    public FunckyValue eval(final ScriptContext context) {
        return value;
    }

    @Override
    public FunckyLiteral normalize() {
        return this;
    }

    @Override
    public String toString() {
        if (value.getType().equals(FunckySimpleType.CHARACTER)) {
            return String.format(FORMAT_CHARACTER, EscapeHelper.escape(value.toString()));
        }
        if (value.getType().equals(FunckyListType.STRING)) {
            return String.format(FORMAT_STRING, EscapeHelper.escape(value.toString()));
        }
        if (value.getType() instanceof FunckyListType) {
            return ((FunckyList) value).toString(FunckyExpression::toString);
        }
        return value.toString();
    }

    @Override
    protected FunckyType getType(final Map<FunckyReference, FunckyTypeVariable> assumptions) {
        return value.getType();
    }
}
