package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;
import java.util.Map;
import java.util.Set;

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
    private static final String JAVA = "new %1$s(engine, %2$s, %3$d, %4$d, %5$s)";
    private static final String JAVA_URI = "%1$s.create(\"%2$s\")";

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
    public FunckyLiteral canonicalize() {
        return this;
    }

    @Override
    public String toJava() {
        return String.format(JAVA, FunckyLiteral.class.getName(), (file == null) ? String.valueOf((Object) null)
                        : String.format(JAVA_URI, URI.class.getName(), EscapeHelper.escape(file.toString())), line,
                column, value.toJava());
    }

    @Override
    public Set<URI> getDependencies() {
        return Set.of();
    }

    @Override
    public FunckyValue eval(final ScriptContext context) {
        return value;
    }

    @Override
    public String toString() {
        if (value.getType().equals(FunckySimpleType.CHARACTER.apply(engine))) {
            return String.format(FORMAT_CHARACTER, EscapeHelper.escape(value.toString()));
        }
        if (value.getType().equals(FunckyListType.STRING.apply(engine))) {
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
