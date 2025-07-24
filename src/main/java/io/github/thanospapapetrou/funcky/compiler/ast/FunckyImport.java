package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;

import io.github.thanospapapetrou.funcky.compiler.parser.EscapeHelper;

public class FunckyImport {
    private static final String FORMAT = "%1$s: \"%2$s\"";

    private final URI file;
    private final int line;
    private final String prefix;
    private final URI namespace;

    public FunckyImport(final URI file, final int line, final String prefix, final URI namespace) {
        this.file = file;
        this.line = line;
        this.prefix = prefix;
        this.namespace = namespace;
    }

    public URI getFile() {
        return file;
    }

    public int getLine() {
        return line;
    }

    public String getPrefix() {
        return prefix;
    }

    public URI getNamespace() {
        return namespace;
    }

    @Override
    public String toString() {
        return String.format(FORMAT, prefix, EscapeHelper.escape(namespace.toString()));
    }
}
