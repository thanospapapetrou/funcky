package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;

import io.github.thanospapapetrou.funcky.compiler.transpiler.Transpiler;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;

public record FunckyDefinition(URI file, int line, String name, FunckyExpression expression) {
    private static final String FORMAT = "%1$s = %2$s";
    private static final String JAVA = "        final %4$s %3$s%1$s = %2$s;%n";

    public String toJava() {
        return String.format(JAVA, name, expression.toJava(), Transpiler.JAVA_PREFIX, FunckyValue.class.getName());
    }

    @Override
    public String toString() {
        return String.format(FORMAT, name, expression);
    }
}
