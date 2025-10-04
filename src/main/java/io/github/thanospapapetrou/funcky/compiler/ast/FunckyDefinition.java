package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;

public record FunckyDefinition(URI file, int line, String name, FunckyExpression expression) {
    private static final String FORMAT = "%1$s = %2$s";
    private static final String JAVA = "    public final io.github.thanospapapetrou.funcky.runtime.FunckyValue $%1$s "
            + "= %2$s;%n";

    public String toJava() {
        return String.format(JAVA, name, expression.toJava());
    }

    @Override
    public String toString() {
        return String.format(FORMAT, name, expression);
    }
}
