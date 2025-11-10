package io.github.thanospapapetrou.funcky.compiler.parser;

import java.net.URI;

public record Import(URI file, int line, String prefix, URI namespace) {
    private static final String FORMAT = "%1$s: \"%2$s\"";

    @Override
    public String toString() {
        return String.format(FORMAT, prefix, EscapeHelper.escape(namespace.toString()));
    }
}
