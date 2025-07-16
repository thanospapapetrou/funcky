package com.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;
import java.util.Map;

import javax.script.ScriptContext;

import com.github.thanospapapetrou.funcky.FunckyEngine;
import com.github.thanospapapetrou.funcky.FunckyFactory;
import com.github.thanospapapetrou.funcky.compiler.parser.EscapeHelper;
import com.github.thanospapapetrou.funcky.runtime.FunckyList;
import com.github.thanospapapetrou.funcky.runtime.FunckyValue;
import com.github.thanospapapetrou.funcky.runtime.types.FunckyListType;
import com.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType;
import com.github.thanospapapetrou.funcky.runtime.types.FunckyType;
import com.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

public class FunckyLiteral extends FunckyExpression {
    private static final String CHARACTER = "'%1$s'";
    private static final String STRING = "\"%1$s\"";

    private final FunckyValue value;

    public FunckyLiteral(final FunckyEngine engine, final URI file, final int line, final int column,
            final FunckyValue value) {
        super(engine, file, line, column);
        this.value = value;
    }

    public FunckyLiteral(final FunckyEngine engine, final FunckyValue value) {
        this(engine, null, -1, -1, value);
    }

    public FunckyLiteral(final FunckyValue value) {
        this(FunckyFactory.ENGINE, value);
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
            return String.format(CHARACTER, EscapeHelper.escape(value.toString()));
        }
        if (value.getType().equals(FunckyListType.STRING)) {
            return String.format(STRING, EscapeHelper.escape(value.toString()));
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
