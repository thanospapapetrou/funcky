package com.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;

public class FunckyDefinition {
    private static final String DEFINITION = "%1$s = %2$s";

    private final URI file;
    private final int line;
    private final String name;
    private final FunckyExpression expression;

    public FunckyDefinition(final URI file, final int line, final String name, final FunckyExpression expression) {
        this.file = file;
        this.line = line;
        this.name = name;
        this.expression = expression;
    }

    public URI getFile() {
        return file;
    }

    public int getLine() {
        return line;
    }

    public String getName() {
        return name;
    }

    public FunckyExpression getExpression() {
        return expression;
    }

    @Override
    public String toString() {
        return String.format(DEFINITION, name, expression);
    }
}
