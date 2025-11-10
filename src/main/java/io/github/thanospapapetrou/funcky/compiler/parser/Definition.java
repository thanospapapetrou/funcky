package io.github.thanospapapetrou.funcky.compiler.parser;

import java.net.URI;

public record Definition(URI file, int line, String name, Expression expression) {
    private static final String FORMAT = "%1$s = %2$s";

    @Override
    public String toString() {
        return String.format(FORMAT, name, expression);
    }
}
