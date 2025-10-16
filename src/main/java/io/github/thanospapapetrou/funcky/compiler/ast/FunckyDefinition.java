package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;
import java.util.Set;

import io.github.thanospapapetrou.funcky.compiler.transpiler.Transpiler;

public record FunckyDefinition(URI file, int line, String name, FunckyExpression expression) {
    private static final String FORMAT = "%1$s = %2$s";
    private static final String JAVA = """
                    %1$s %2$s%3$s = %4$s;
            """;

    public String toJava() {
        return String.format(JAVA, FunckyExpression.class.getName(), Transpiler.JAVA_DELIMITER, name, expression.toJava());
    }

    public Set<URI> getDependencies() {
        return expression.getDependencies();
    }

    @Override
    public String toString() {
        return String.format(FORMAT, name, expression);
    }
}
