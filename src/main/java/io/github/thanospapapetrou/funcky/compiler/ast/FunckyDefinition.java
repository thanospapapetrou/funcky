package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;

public record FunckyDefinition(URI file, int line, String name, FunckyExpression expression) {
    private static final String FORMAT = "%1$s = %2$s";

    @Override
    public String toString() {
        return String.format(FORMAT, name, expression);
    }
}
