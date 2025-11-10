package io.github.thanospapapetrou.funcky.compiler.parser;

import java.net.URI;

public final class Reference extends Expression {
    private static final String FORMAT_NAMESPACE = "\"%1$s\".%2$s";
    private static final String FORMAT_PREFIX = "%1$s.%2$s";

    private final URI namespace;
    private final String prefix;
    private final String name;

    Reference(final URI file, final int line, final int column, final URI namespace, final String name) {
        this(file, line, column, namespace, null, name);
    }

    Reference(final URI file, final int line, final int column, final String prefix, final String name) {
        this(file, line, column, null, prefix, name);
    }

    Reference(final URI file, final int line, final int column, final String name) {
        this(file, line, column, null, null, name);
    }

    private Reference(final URI file, final int line, final int column, final URI namespace, final String prefix,
            final String name) {
        super(file, line, column);
        this.namespace = namespace;
        this.prefix = prefix;
        this.name = name;
    }

    public URI getNamespace() {
        return namespace;
    }

    public String getPrefix() {
        return prefix;
    }

    public String getName() {
        return name;
    }

    @Override
    public String toString() {
        return (namespace == null) ? ((prefix == null)
                ? name
                : String.format(FORMAT_PREFIX, prefix, name))
                : String.format(FORMAT_NAMESPACE, EscapeHelper.escape(namespace.toString()), name);
    }
}
